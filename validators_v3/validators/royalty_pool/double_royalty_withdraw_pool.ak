use aiken/builtin
use aiken/collection/list
use aiken/crypto.{blake2b_224}
use aiken/primitive/bytearray
use cardano/address.{Address, Credential, Script}
use cardano/assets.{ada_policy_id}
use cardano/transaction.{InlineDatum, Input, Output, Transaction}
use splash/orders/royalty_withdraw.{
  DoubleRoyaltyWithdrawRedeemer, RoyaltyWithdrawConfig, WithdrawData,
  WithdrawRoyaltyDataToSign,
}
use splash/plutus.{Asset}
use splash/royalty_pool/double_royalty_pool.{DoubleRoyaltyPoolConfig}
use splash/value_ext.{asset_quantity_of, v_len}

// Double Royalty Withdrawal Contract
// 
// Purpose
// - Securely process withdrawals from a pool that maintains two royalty slots
//   (first/second). Enforces selection by signer, correct value deltas,
//   and datum immutability.
//
// Enforced rules
// 1) Restricted Field Modifications
//    - Only these fields may change:
//        • first_royalty_x / first_royalty_y  (when signer == 0)
//        • second_royalty_x / second_royalty_y (when signer != 0)
//        • nonce (must increase by exactly +1)
//    - All other fields (pool_x, pool_y, pool_lq, fees, dao_policy,
//      treasury_address, *_pub_key, etc.) must be unchanged.
//
// 2) Signer delection
//    - signer == 0 → withdraw from the first slot:
//        new.first_royalty_x = old.first_royalty_x - withdraw_x
//        new.first_royalty_y = old.first_royalty_y - withdraw_y
//        new.second_* == old.second_*
//    - signer != 0 → withdraw from the second slot:
//        new.second_royalty_x = old.second_royalty_x - withdraw_x
//        new.second_royalty_y = old.second_royalty_y - withdraw_y
//        new.first_* == old.first_*
//    - Bounds: withdraw_x ≤ slot_x and withdraw_y ≤ slot_y.
//
// 3) Value deltas (outputs vs inputs)
//    - pool_x and pool_y in the pool output value must be reduced exactly by
//      withdraw_x / withdraw_y.
//    - pool_lq must be unchanged.
//
// 4) ADA invariant for token-to-token pools
//    - If both pool_x and pool_y are non-ADA (policy != ada_policy_id), then
//      the lovelace amount in the pool output must equal the input’s.
//
// 5) Pool identity, address & shape
//    - Pool output is located by pool_nft quantity == 1.
//    - The pool address (input → output) must remain the same.
//    - The “shape” of the value (token entry count) must remain unchanged.
//
// 6) Input structure
//    - Exactly two inputs: the withdraw-request UTxO and the pool UTxO.
//    - The withdraw-request input must be locked by royalty_withdraw_request_vh
//      (script-hash check) to prevent spoofed requests.
//
// 7) Signature verification
//    - Message = additional_bytes || payload_to_sign
//    - payload_to_sign is the CBOR of:
//        WithdrawRoyaltyDataToSign { withdraw_data, pool_nonce = old.nonce }
//      (or its blake2b_224 hash when the redeemer’s flag requests hashing).
//    - Public key selection:
//        signer == 0 → first_royalty_pub_key
//        otherwise   → second_royalty_pub_key
//
// 8) Final datum check
//    - The new DoubleRoyaltyPoolConfig must equal the previous one except for:
//        • the modified royalty slot fields (per signer rule), and
//        • nonce = old.nonce + 1.


const royalty_withdraw_request_vh =
  #"6710f3002759d028a90a08e1538b25d4eaf48c72d88157f71d68dcc0"

validator double_royalty_withdraw_pool {
  withdraw(
    redeemer: DoubleRoyaltyWithdrawRedeemer,
    _account: Credential,
    self: Transaction,
  ) {
    let inputs = self.inputs
    let outputs = self.outputs

    let order_in_ix = redeemer.order_in_ix
    let pool_in_ix = redeemer.pool_in_ix
    let signer = redeemer.signer
    let hash = redeemer.hash

    expect Some(order_input) = list.at(inputs, order_in_ix)
    let Input { output: order_input_out, .. } = order_input

    expect Output {
      address: Address { payment_credential: order_pc, .. },
      datum: InlineDatum(order_datum),
      ..
    } = order_input_out

    let correct_order_input_address =
      when order_pc is {
        Script(vh) ->
          vh == royalty_withdraw_request_vh && list.length(inputs) == 2
        _ -> False
      }

    expect order_conf: RoyaltyWithdrawConfig = order_datum
    let RoyaltyWithdrawConfig { withdraw_data: wd, signature, additional_bytes } =
      order_conf

    let WithdrawData {
      pool_nft: pool_nft_req,
      withdraw_royalty_x: withdraw_x,
      withdraw_royalty_y: withdraw_y,
      ..
    } = wd

    expect Some(pool_input) = list.at(inputs, pool_in_ix)
    let Input { output: pool_in_output, .. } = pool_input

    expect Output {
      value: prev_pool_value,
      address: prev_addr,
      datum: InlineDatum(pool_in_datum),
      ..
    } = pool_in_output

    expect prev_cfg: DoubleRoyaltyPoolConfig = pool_in_datum

    let DoubleRoyaltyPoolConfig {
      pool_nft: prev_pool_nft,
      pool_x: prev_pool_x,
      pool_y: prev_pool_y,
      pool_lq: prev_pool_lq,
      fee_num: prev_fee_num,
      treasury_fee: prev_treasury_fee,
      first_royalty_fee: prev_first_fee,
      second_royalty_fee: prev_second_fee,
      treasury_x: prev_treasury_x,
      treasury_y: prev_treasury_y,
      first_royalty_x: prev_first_rx,
      first_royalty_y: prev_first_ry,
      second_royalty_x: prev_second_rx,
      second_royalty_y: prev_second_ry,
      dao_policy: prev_dao_policy,
      treasury_address: prev_treasury_address,
      first_royalty_pub_key: prev_first_pk,
      second_royalty_pub_key: prev_second_pk,
      nonce: prev_nonce,
    } = prev_cfg

    let prev_x_val = asset_quantity_of(prev_pool_value, prev_pool_x)
    let prev_y_val = asset_quantity_of(prev_pool_value, prev_pool_y)
    let prev_lq_val = asset_quantity_of(prev_pool_value, prev_pool_lq)

    let prev_lovelace_token2token =
      if prev_pool_x.policy == ada_policy_id || prev_pool_y.policy == ada_policy_id {
        0
      } else {
        assets.lovelace_of(prev_pool_value)
      }

    let prev_len = v_len(prev_pool_value)
    let correct_pool = prev_pool_nft == pool_nft_req

    expect Some(pool_out_output) =
      list.find(
        outputs,
        fn(o) { asset_quantity_of(o.value, prev_pool_nft) == 1 },
      )

    expect Output {
      value: new_value,
      address: new_addr,
      datum: InlineDatum(pool_out_datum),
      ..
    } = pool_out_output

    expect new_cfg: DoubleRoyaltyPoolConfig = pool_out_datum

    let DoubleRoyaltyPoolConfig {
      first_royalty_x: new_first_rx,
      first_royalty_y: new_first_ry,
      second_royalty_x: new_second_rx,
      second_royalty_y: new_second_ry,
      ..
    } = new_cfg

    let new_x_val = asset_quantity_of(new_value, prev_pool_x)
    let new_y_val = asset_quantity_of(new_value, prev_pool_y)
    let new_lq_val = asset_quantity_of(new_value, prev_pool_lq)

    let new_lovelace_token2token =
      if prev_pool_x.policy == ada_policy_id || prev_pool_y.policy == ada_policy_id {
        0
      } else {
        assets.lovelace_of(new_value)
      }

    let new_len = v_len(new_value)

    let correct_final_pool_address = prev_addr == new_addr
    let correct_tokens_qty_in_pool = prev_len == new_len
    let pool_identity = asset_quantity_of(new_value, prev_pool_nft) == 1

    let royalty_withdraw_is_correct =
      if signer == 0 {
        withdraw_x <= prev_first_rx && withdraw_y <= prev_first_ry && new_first_rx == prev_first_rx - withdraw_x && new_first_ry == prev_first_ry - withdraw_y && prev_second_rx == new_second_rx && prev_second_ry == new_second_ry
      } else {
        withdraw_x <= prev_second_rx && withdraw_y <= prev_second_ry && new_second_rx == prev_second_rx - withdraw_x && new_second_ry == prev_second_ry - withdraw_y && prev_first_rx == new_first_rx && prev_first_ry == new_first_ry
      }

    let tokens_diff_is_correct =
      prev_lq_val == new_lq_val && prev_x_val - withdraw_x == new_x_val && prev_y_val - withdraw_y == new_y_val

    let correct_lovelace_token2token =
      prev_lovelace_token2token == new_lovelace_token2token

    let payload: WithdrawRoyaltyDataToSign =
      WithdrawRoyaltyDataToSign { withdraw_data: wd, pool_nonce: prev_nonce }
    let payload_raw = builtin.serialise_data(payload)
    let payload_to_sign =
      if hash {
        blake2b_224(payload_raw)
      } else {
        payload_raw
      }
    let message = bytearray.concat(additional_bytes, payload_to_sign)
    let signer_key =
      if signer == 0 {
        prev_first_pk
      } else {
        prev_second_pk
      }
    let signature_is_correct =
      crypto.verify_ed25519_signature(signer_key, message, signature)

    let expected_new_cfg: DoubleRoyaltyPoolConfig =
      DoubleRoyaltyPoolConfig {
        pool_nft: prev_pool_nft,
        pool_x: prev_pool_x,
        pool_y: prev_pool_y,
        pool_lq: prev_pool_lq,
        fee_num: prev_fee_num,
        treasury_fee: prev_treasury_fee,
        first_royalty_fee: prev_first_fee,
        second_royalty_fee: prev_second_fee,
        treasury_x: prev_treasury_x,
        treasury_y: prev_treasury_y,
        first_royalty_x: new_first_rx,
        first_royalty_y: new_first_ry,
        second_royalty_x: new_second_rx,
        second_royalty_y: new_second_ry,
        dao_policy: prev_dao_policy,
        treasury_address: prev_treasury_address,
        first_royalty_pub_key: prev_first_pk,
        second_royalty_pub_key: prev_second_pk,
        nonce: prev_nonce + 1,
      }

    let final_pool_config_is_correct = expected_new_cfg == new_cfg

    let strict_inputs = list.length(inputs) == 2

    correct_order_input_address && correct_final_pool_address && correct_tokens_qty_in_pool && pool_identity && royalty_withdraw_is_correct && tokens_diff_is_correct && signature_is_correct &&
    final_pool_config_is_correct && correct_pool && correct_lovelace_token2token && strict_inputs
  }

  else(_) {
    fail
  }
}
