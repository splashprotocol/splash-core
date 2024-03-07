use aiken/list
use aiken/transaction.{InlineDatum, ScriptContext}
use aiken/transaction/credential.{ScriptCredential, VerificationKeyCredential}
use aiken/transaction/value.{quantity_of}
use splash/stableswap/types.{
  ApplyOrder, CancelOrder, OrderAction, PoolData, RedeemData,
}
use splash/stableswap/utils

validator {
  fn redeem(datum: RedeemData, action: OrderAction, ctx: ScriptContext) -> Bool {
    when action is {
      ApplyOrder { redeemer_in_ix, redeemer_out_ix, pool_in_ix } -> {
        // Input order data:
        expect Some(self_input) =
          list.at(ctx.transaction.inputs, redeemer_in_ix)
        let self = self_input.output

        // Input pool data:
        expect Some(pool_input) = list.at(ctx.transaction.inputs, pool_in_ix)
        let pool = pool_input.output
        expect pool_data: PoolData =
          when pool.datum is {
            InlineDatum(d) -> d
            _ -> utils.find_script_datum(ctx.transaction.datums, pool.datum)
          }

        // Output user data:
        expect Some(user_output) =
          list.at(ctx.transaction.outputs, redeemer_out_ix)

        // Validations:
        // 1. Redeemed from the valid pool:
        let valid_pool: Bool =
          quantity_of(pool.value, datum.pool_nft.policy, datum.pool_nft.name) == 1
        // 2. Valid expected tokens (and their amount in user's output):
        let received_amounts =
          utils.extract_target_balances(
            datum.expected_assets,
            user_output.value,
          )
        // Ordering of "min_expected_received_assets_balances" and assets in the user output must be the same.
        let valid_amounts_received: Bool =
          list.all(
            list.map2(
              received_amounts,
              datum.min_expected_received_assets_balances,
              fn(a, b) { a >= b },
            ),
            fn(x) { x == True },
          )

        // 3. Valid change:
        let lp_change =
          quantity_of(
            user_output.value,
            pool_data.lp_token.policy,
            pool_data.lp_token.name,
          )
        let valid_lp_change: Bool = lp_change == datum.min_expected_lp_change

        // 4. Valid successor:
        let valid_successor: Bool =
          when user_output.address.payment_credential is {
            VerificationKeyCredential(successor_cred) ->
              datum.redeemer == successor_cred
            ScriptCredential(_) -> self.address == user_output.address
          }

        valid_pool && valid_amounts_received && valid_lp_change && valid_successor
      }
      CancelOrder -> list.has(ctx.transaction.extra_signatories, datum.redeemer)
    }
  }
}
