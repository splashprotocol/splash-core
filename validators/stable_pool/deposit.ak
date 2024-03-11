use aiken/list
use aiken/transaction.{ScriptContext}
use aiken/transaction/credential.{ScriptCredential, VerificationKeyCredential}
use aiken/transaction/value.{quantity_of}
use splash/stableswap/types.{
  ApplyOrder, CancelOrder, DepositData, OrderAction, PoolData,
}
use splash/stableswap/utils

validator {
  fn deposit(
    datum: DepositData,
    action: OrderAction,
    ctx: ScriptContext,
  ) -> Bool {
    when action is {
      ApplyOrder { redeemer_in_ix, redeemer_out_ix, pool_in_ix } -> {
        // Input order data:
        expect Some(self_input) =
          list.at(ctx.transaction.inputs, redeemer_in_ix)
        let self = self_input.output

        // Input pool data:
        expect Some(pool_input) = list.at(ctx.transaction.inputs, pool_in_ix)
        let pool = pool_input.output
        expect pool_data: PoolData = utils.extract_datum(pool.datum)

        // Output user data:
        expect Some(user_output) =
          list.at(ctx.transaction.outputs, redeemer_out_ix)

        // Validations:
        // 1. Deposited into the valid pool:
        let valid_pool: Bool =
          quantity_of(pool.value, datum.pool_nft.policy, datum.pool_nft.name) == 1
        // 2. Valid expected tokens (and their amount in user's output):
        let received_lp_amount =
          quantity_of(
            user_output.value,
            pool_data.lp_token.policy,
            pool_data.lp_token.name,
          )

        let valid_lp_amount_received: Bool =
          received_lp_amount >= datum.min_expected_lp_amount

        // 3. Valid successor:
        let valid_successor: Bool =
          when user_output.address.payment_credential is {
            VerificationKeyCredential(successor_cred) ->
              datum.redeemer == successor_cred
            ScriptCredential(_) -> self.address == user_output.address
          }

        valid_pool && valid_lp_amount_received && valid_successor
      }
      CancelOrder -> list.has(ctx.transaction.extra_signatories, datum.redeemer)
    }
  }
}
