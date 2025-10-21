use aiken/builtin
use aiken/collection/list
use aiken/crypto.{blake2b_224}
use aiken/primitive/bytearray
use cardano/address.{Address, Credential, Script}
use cardano/assets.{ada_policy_id}
use cardano/transaction.{InlineDatum, Input, Output, Transaction}
use splash/orders/royalty_withdraw.{
  RoyaltyWithdrawConfig, RoyaltyWithdrawRedeemer, WithdrawData,
  WithdrawRoyaltyDataToSign,
}
use splash/royalty_pool/single_royalty_pool.{SingleRoyaltyPoolConfig}
use splash/value_ext.{asset_quantity_of, v_len}

// Single Royalty Withdrawal Contract (Aiken)
//
// Purpose
// - Securely process royalty withdrawals from a pool with a single royalty slot.
//   Enforces correctness, immutability, and authorization.
//
// Enforced rules
// 1) Restricted Field Modifications
//    - Only these fields may change:
//        • royalty_x, royalty_y
//        • nonce (must increase by exactly +1)
//    - All other fields (pool_x, pool_y, pool_lq, fees, dao_policy,
//      treasury_address, royalty_pub_key, etc.) must be unchanged.
//
// 2) Withdrawal Limits
//    - Requested withdraw_x/withdraw_y must not exceed the current
//      royalty_x/royalty_y in the input datum.
//
// 3) Input Structure
//    - Exactly two inputs: (1) the withdraw-request UTxO and (2) the pool UTxO.
//    - The withdraw-request input must be locked by the expected script hash
//      (royalty_withdraw_request_vh) to prevent spoofed requests.
//
// 4) Pool Identity, Address & Output Selection
//    - The pool output is identified by pool_nft quantity == 1.
//    - The pool address (input → output) must remain the same.
//
// 5) Value Deltas (outputs vs inputs)
//    - pool_x and pool_y quantities in the pool output value must be reduced
//      exactly by withdraw_x / withdraw_y.
//    - pool_lq quantity must remain unchanged.
//
// 6) ADA Invariant for Token-to-Token Pools
//    - If both pool_x and pool_y are non-ADA (policy != ada_policy_id),
//      the lovelace amount in the pool output must equal the input’s.
//    - The “shape” of the value (token entry count) should remain unchanged.
//
// 7) Signature Verification
//    - Message = additional_bytes || CBOR(WithdrawRoyaltyDataToSign
//      { withdraw_data, pool_nonce = old.nonce }).
//    - The signature must verify against royalty_pub_key.
//
// 8) Final Datum Check
//    - The new RoyaltyPoolConfig must equal the previous one except for:
//        • royalty_x / royalty_y updated by the withdrawal amounts, and
//        • nonce = old.nonce + 1.

const royalty_withdraw_request_vh =
  #"92c094b90cf3637a96a13e9bc9a04ce8bb7e48c7ed0b5d1cc5ca7332"

validator royalty_withdraw_pool {
  withdraw(
    redeemer: RoyaltyWithdrawRedeemer,
    _account: Credential,
    self: Transaction,
  ) {
    let inputs = self.inputs
    let outputs = self.outputs

    let order_in_ix = redeemer.order_in_ix
    let pool_in_ix = redeemer.pool_in_ix
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
      value: prev_value,
      address: prev_addr,
      datum: InlineDatum(pool_in_datum),
      ..
    } = pool_in_output

    expect prev_cfg: SingleRoyaltyPoolConfig = pool_in_datum

    let SingleRoyaltyPoolConfig {
      pool_nft: prev_pool_nft,
      pool_x: prev_pool_x,
      pool_y: prev_pool_y,
      pool_lq: prev_pool_lq,
      fee_num: prev_fee_num,
      treasury_fee: prev_treasury_fee,
      royalty_fee: prev_royalty_fee,
      treasury_x: prev_treasury_x,
      treasury_y: prev_treasury_y,
      royalty_x: prev_rx,
      royalty_y: prev_ry,
      dao_policy: prev_dao_policy,
      treasury_address: prev_treasury_address,
      royalty_pub_key: prev_pk,
      nonce: prev_nonce,
    } = prev_cfg

    let prev_x_val = asset_quantity_of(prev_value, prev_pool_x)
    let prev_y_val = asset_quantity_of(prev_value, prev_pool_y)
    let prev_lq_val = asset_quantity_of(prev_value, prev_pool_lq)

    let prev_lovelace_token2token =
      if prev_pool_x.policy == ada_policy_id || prev_pool_y.policy == ada_policy_id {
        0
      } else {
        assets.lovelace_of(prev_value)
      }

    let prev_len = v_len(prev_value)
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

    expect new_cfg: SingleRoyaltyPoolConfig = pool_out_datum

    let SingleRoyaltyPoolConfig { royalty_x: new_rx, royalty_y: new_ry, .. } =
      new_cfg

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

    let royalty_withdraw_ok =
      withdraw_x <= prev_rx && withdraw_y <= prev_ry && new_rx == prev_rx - withdraw_x && new_ry == prev_ry - withdraw_y && prev_lq_val == new_lq_val && prev_x_val - withdraw_x == new_x_val && prev_y_val - withdraw_y == new_y_val

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
    let signature_is_correct =
      crypto.verify_ed25519_signature(prev_pk, message, signature)

    let expected_new_cfg: SingleRoyaltyPoolConfig =
      SingleRoyaltyPoolConfig {
        pool_nft: prev_pool_nft,
        pool_x: prev_pool_x,
        pool_y: prev_pool_y,
        pool_lq: prev_pool_lq,
        fee_num: prev_fee_num,
        treasury_fee: prev_treasury_fee,
        royalty_fee: prev_royalty_fee,
        treasury_x: prev_treasury_x,
        treasury_y: prev_treasury_y,
        royalty_x: new_rx,
        royalty_y: new_ry,
        dao_policy: prev_dao_policy,
        treasury_address: prev_treasury_address,
        royalty_pub_key: prev_pk,
        nonce: prev_nonce + 1,
      }

    let final_pool_config_is_correct = expected_new_cfg == new_cfg

    let strict_inputs = list.length(inputs) == 2

    correct_order_input_address && correct_final_pool_address && correct_tokens_qty_in_pool && pool_identity && royalty_withdraw_ok && signature_is_correct && strict_inputs && final_pool_config_is_correct && correct_pool && correct_lovelace_token2token
  }

  else(_) {
    fail
  }
}
