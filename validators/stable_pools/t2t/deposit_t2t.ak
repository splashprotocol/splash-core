use aiken/list
use aiken/transaction.{InlineDatum, Output, ScriptContext, Transaction}
use aiken/transaction/credential.{Inline, VerificationKeyCredential}
use aiken/transaction/value.{quantity_of}
use splash/plutus.{Asset}
use splash/stableswap/types.{
  ApplyOrder, CancelOrder, DepositData, OrderAction, PoolDataT2T,
}

// LP tokens emisson:
const lp_emission = 9223372036854775807

validator {
  fn stable_deposit_t2t(
    datum: DepositData,
    action: OrderAction,
    ctx: ScriptContext,
  ) -> Bool {
    let ScriptContext { transaction, .. } = ctx
    when action is {
      ApplyOrder { redeemer_in_ix, redeemer_out_ix, pool_in_ix } -> {
        let DepositData {
          redeemer_pk,
          redeemer_sc,
          pool_nft,
          min_expected_lp_amount,
          ex_fee,
        } = datum

        let Transaction { inputs, outputs, .. } = transaction

        // Input pool data:
        expect Some(pool_input) = list.at(inputs, pool_in_ix)
        expect Output {
          value: pool_value,
          datum: InlineDatum(pool_datum_in),
          ..
        } = pool_input.output
        expect pool_datum: PoolDataT2T = pool_datum_in
        let PoolDataT2T {
          lp_token,
          asset_x,
          asset_y,
          protocol_fees_x,
          protocol_fees_y,
          ..
        } = pool_datum
        let Asset { policy: asset_x_policy, name: asset_x_name } = asset_x
        let Asset { policy: asset_y_policy, name: asset_y_name } = asset_y
        let Asset { policy: lp_token_policy, name: lp_token_name } = lp_token

        let x_total0 =
          value.quantity_of(pool_value, asset_x_policy, asset_x_name) - protocol_fees_x
        let y_total0 =
          value.quantity_of(pool_value, asset_y_policy, asset_y_name) - protocol_fees_y
        let lp_total0 =
          value.quantity_of(pool_value, lp_token_policy, lp_token_name)

        // Input order data:
        expect Some(self_input) = list.at(inputs, redeemer_in_ix)
        let Output { value: self_value, .. } = self_input.output
        let Asset { policy: pool_nft_policy, name: pool_nft_name } = pool_nft
        let x_order = quantity_of(self_value, asset_x_policy, asset_x_name)
        let y_order = quantity_of(self_value, asset_y_policy, asset_y_name)

        // Output user data:
        expect Some(user_output) = list.at(outputs, redeemer_out_ix)
        let Output { value, address: user_output_address, .. } = user_output
        let received_lp_amount =
          quantity_of(value, lp_token_policy, lp_token_name)
        let received_x_change = quantity_of(value, asset_x_policy, asset_x_name)
        let received_y_change = quantity_of(value, asset_y_policy, asset_y_name)
        let x_delta = x_order - received_x_change
        let y_delta = y_order - received_y_change
        let (x_order_effective, y_order_effective) =
          if asset_x_policy == #"" && asset_x_name == #"" {
            (x_delta - ex_fee, y_delta)
          } else if asset_y_policy == #"" && asset_y_name == #"" {
            (x_delta, y_delta - ex_fee)
          } else {
            (x_delta, y_delta)
          }

        // Validations:
        // 1. Deposited into the valid pool:
        let valid_pool: Bool =
          quantity_of(pool_value, pool_nft_policy, pool_nft_name) == 1
        // 2. Valid expected token amount in user's output:
        let min_expected_lp_amount_received: Bool =
          received_lp_amount >= min_expected_lp_amount
        // 3. Valid inputs:
        let valid_inputs: Bool = list.length(inputs) == 2
        // 4. Valid change:
        let supply_lp0 = lp_emission - lp_total0
        let valid_change: Bool =
          received_lp_amount * x_total0 >= supply_lp0 * x_order_effective && received_lp_amount * y_total0 >= supply_lp0 * y_order_effective
        // 5. Valid successor:
        expect VerificationKeyCredential(successor_pay_cred) =
          user_output_address.payment_credential
        expect Some(Inline(VerificationKeyCredential(successor_stake_cred))) =
          user_output_address.stake_credential
        let valid_successor: Bool =
          successor_pay_cred == redeemer_pk && successor_stake_cred == redeemer_sc

        valid_pool && min_expected_lp_amount_received && valid_inputs && valid_change && valid_successor
      }
      CancelOrder -> {
        let DepositData { redeemer_pk, .. } = datum
        let Transaction { extra_signatories, .. } = transaction
        list.has(extra_signatories, redeemer_pk)
      }
    }
  }
}
