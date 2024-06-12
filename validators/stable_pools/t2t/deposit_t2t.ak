use aiken/list
use aiken/transaction.{InlineDatum, Output, ScriptContext, Transaction}
use aiken/transaction/credential.{ScriptCredential, VerificationKeyCredential}
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
    let Transaction { extra_signatories, .. } = transaction
    let DepositData { redeemer, .. } = datum

    when action is {
      ApplyOrder { redeemer_in_ix, redeemer_out_ix, pool_in_ix } -> {
        let Transaction { inputs, outputs, .. } = transaction

        // Input pool data:
        expect Some(pool_input) = list.at(inputs, pool_in_ix)
        expect Output {
          value: pool_value,
          datum: InlineDatum(pool_datum_in),
          ..
        } = pool_input.output
        expect pool_datum: PoolDataT2T = pool_datum_in
        let PoolDataT2T { lp_token, asset_x, asset_y, .. } = pool_datum
        let Asset { policy: asset_x_policy, name: asset_x_name } = asset_x
        let Asset { policy: asset_y_policy, name: asset_y_name } = asset_y
        let Asset { policy: lp_token_policy, name: lp_token_name } = lp_token

        let x_total0 =
          value.quantity_of(pool_value, asset_x_policy, asset_x_name)
        let y_total0 =
          value.quantity_of(pool_value, asset_y_policy, asset_y_name)
        let lp_total0 =
          value.quantity_of(pool_value, lp_token_policy, lp_token_name)

        // Input order data:
        expect Some(self_input) = list.at(inputs, redeemer_in_ix)
        let DepositData { pool_nft, min_expected_lp_amount, .. } = datum
        let Output { value: self_value, .. } = self_input.output
        let Asset { policy: pool_nft_policy, name: pool_nft_name } = pool_nft
        let x_order = quantity_of(self_value, asset_x_policy, asset_x_name)
        let y_order = quantity_of(self_value, asset_y_policy, asset_y_name)

        // Output user data:
        expect Some(user_output) = list.at(outputs, redeemer_out_ix)
        let Output { value, address, .. } = user_output
        let received_lp_amount =
          quantity_of(value, lp_token_policy, lp_token_name)
        let received_x_change = quantity_of(value, asset_x_policy, asset_x_name)
        let received_y_change = quantity_of(value, asset_y_policy, asset_y_name)

        // Validations:
        // 1. Deposited into the valid pool:
        let valid_pool: Bool =
          quantity_of(pool_value, pool_nft_policy, pool_nft_name) == 1
        // 2. Valid expected token amount in user's output:
        let min_expected_lp_amount_received: Bool =
          received_lp_amount >= min_expected_lp_amount
        // 3. Valid change:
        let supply_lp0 = lp_emission - lp_total0
        let valid_change: Bool =
          received_lp_amount * x_total0 >= supply_lp0 * (
            x_order - received_x_change
          ) && received_lp_amount * y_total0 >= supply_lp0 * (
            y_order - received_y_change
          )
        // 4. Valid successor:
        let valid_successor: Bool =
          when user_output.address.payment_credential is {
            VerificationKeyCredential(successor_cred) ->
              redeemer == successor_cred
            ScriptCredential(_) -> self_input.output.address == address
          }

        valid_pool && min_expected_lp_amount_received && valid_change && valid_successor
      }
      CancelOrder -> list.has(extra_signatories, redeemer)
    }
  }
}
