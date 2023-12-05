use aiken/list
use aiken/transaction.{ScriptContext, Spend, find_input}
use aiken/transaction/credential.{ScriptCredential, VerificationKeyCredential}
use aiken/transaction/value.{quantity_of}
use whalepools/stable3pool/types.{CancelSwap, ExecSwap, SwapAction, SwapData}
use whalepools/stable3pool/utils.{convert_swap_datum}

validator {
  fn swap(datum: SwapData, action: SwapAction, ctx: ScriptContext) -> Bool {
    when action is {
      ExecSwap { pool_in_ix, successor_ix } -> {
        let ScriptContext { transaction, purpose } = ctx
        expect Spend(spent_utxo_reference) = purpose
        // Input data:
        expect Some(self_input) =
          find_input(transaction.inputs, spent_utxo_reference)
        expect Some(pool_input) = list.at(transaction.inputs, pool_in_ix)
        let pool0 = pool_input.output
        let self = self_input.output

        let self_datum: SwapData = convert_swap_datum(self.datum)

        // Output data:
        expect Some(user_output) = list.at(transaction.outputs, successor_ix)

        // Validations:
        // 1. Deposited into the valid pool:
        let valid_pool =
          quantity_of(pool0.value, datum.pool_nft.policy, datum.pool_nft.name) == 1
        // 2. Valid expected tokens (and their amount in user's output):
        let received_quote_amount =
          quantity_of(
            user_output.value,
            self_datum.expected_quote_token.policy,
            self_datum.expected_quote_token.name,
          )

        let valid_received_quote_amount =
          received_quote_amount >= self_datum.min_expected_quote_amount
        // 3. Valid successor:
        let valid_successor =
          when user_output.address.payment_credential is {
            VerificationKeyCredential(successor_cred) ->
              self_datum.redeemer == successor_cred
            ScriptCredential(_) -> self.address == user_output.address
          }

        valid_pool && valid_received_quote_amount && valid_successor
      }
      CancelSwap -> list.has(ctx.transaction.extra_signatories, datum.redeemer)
    }
  }
}
