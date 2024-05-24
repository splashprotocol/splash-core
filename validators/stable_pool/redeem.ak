use aiken/list
use aiken/transaction.{InlineDatum, Output, ScriptContext, Transaction}
use aiken/transaction/credential.{ScriptCredential, VerificationKeyCredential}
use aiken/transaction/value.{quantity_of}
use splash/plutus.{Asset}
use splash/stableswap/types.{
  ApplyOrder, CancelOrder, OrderAction, PoolData, RedeemData,
}
use splash/stableswap/utils

validator {
  fn stable_redeem(
    datum: RedeemData,
    action: OrderAction,
    ctx: ScriptContext,
  ) -> Bool {
    let ScriptContext { transaction, .. } = ctx
    let Transaction { extra_signatories, .. } = transaction
    let RedeemData { redeemer, .. } = datum

    when action is {
      ApplyOrder { redeemer_in_ix, redeemer_out_ix, pool_in_ix } -> {
        let Transaction { inputs, outputs, .. } = transaction
        // Input order data:
        expect Some(self_input) = list.at(inputs, redeemer_in_ix)
        let RedeemData {
          pool_nft,
          expected_assets,
          min_expected_received_assets_balances,
          min_expected_lp_change,
          ..
        } = datum

        let Asset { policy: pool_nft_policy, name: pool_nft_name } = pool_nft

        // Input pool data:
        expect Some(pool_input) = list.at(inputs, pool_in_ix)
        expect Output { value: pool_value, datum: InlineDatum(pool_datum), .. } =
          pool_input.output
        expect PoolData { lp_token, .. }: PoolData = pool_datum
        let Asset { policy: lp_token_policy, name: lp_token_name } = lp_token

        // Output user data:
        expect Some(user_output) = list.at(outputs, redeemer_out_ix)
        let Output { value, address, .. } = user_output

        // Validations:
        // 1. Redeemed from the valid pool:
        let valid_pool: Bool =
          quantity_of(pool_value, pool_nft_policy, pool_nft_name) == 1
        // 2. Valid expected tokens (and their amount in user's output):
        let received_amounts =
          utils.extract_target_balances(expected_assets, value)
        // Ordering of "min_expected_received_assets_balances" and assets in the user output must be the same.
        let valid_amounts_received: Bool =
          list.all(
            list.map2(
              received_amounts,
              min_expected_received_assets_balances,
              fn(a, b) { a >= b },
            ),
            fn(x) { x == True },
          )

        // 3. Valid change:
        let lp_change = quantity_of(value, lp_token_policy, lp_token_name)
        let valid_lp_change: Bool = lp_change == min_expected_lp_change

        // 4. Valid successor:
        let valid_successor: Bool =
          when address.payment_credential is {
            VerificationKeyCredential(successor_cred) ->
              redeemer == successor_cred
            ScriptCredential(_) -> self_input.output.address == address
          }

        valid_pool && valid_amounts_received && valid_lp_change && valid_successor
      }
      CancelOrder -> list.has(extra_signatories, redeemer)
    }
  }
}
