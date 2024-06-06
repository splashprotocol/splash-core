use aiken/list
use aiken/transaction.{InlineDatum, Output, ScriptContext, Transaction}
use aiken/transaction/credential.{ScriptCredential, VerificationKeyCredential}
use aiken/transaction/value.{quantity_of}
use splash/plutus.{Asset}
use splash/stableswap/types.{
  ApplyOrder, CancelOrder, DepositData, OrderAction, PoolDataT2T2T,
}

validator {
  fn stable_deposit_t2t2t(
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

        // Input order data:
        expect Some(self_input) = list.at(inputs, redeemer_in_ix)
        let DepositData { pool_nft, min_expected_lp_amount, .. } = datum
        let Asset { policy: pool_nft_policy, name: pool_nft_name } = pool_nft

        // Input pool data:
        expect Some(pool_input) = list.at(inputs, pool_in_ix)
        expect Output {
          value: pool_value,
          datum: InlineDatum(pool_datum_in),
          ..
        } = pool_input.output
        expect pool_datum: PoolDataT2T2T = pool_datum_in
        let PoolDataT2T2T { lp_token, .. } = pool_datum

        let Asset { policy: lp_token_policy, name: lp_token_name } = lp_token

        // Output user data:
        expect Some(user_output) = list.at(outputs, redeemer_out_ix)
        let Output { value, address, .. } = user_output
        let received_lp_amount =
          quantity_of(value, lp_token_policy, lp_token_name)
        // Validations:
        // 1. Deposited into the valid pool:
        let valid_pool: Bool =
          quantity_of(pool_value, pool_nft_policy, pool_nft_name) == 1
        // 2. Valid expected token amount in user's output:
        let min_expected_lp_amount_received: Bool =
          received_lp_amount >= min_expected_lp_amount
        // 3. Valid successor:
        let valid_successor: Bool =
          when user_output.address.payment_credential is {
            VerificationKeyCredential(successor_cred) ->
              redeemer == successor_cred
            ScriptCredential(_) -> self_input.output.address == address
          }

        valid_pool && min_expected_lp_amount_received && valid_successor
      }
      CancelOrder -> list.has(extra_signatories, redeemer)
    }
  }
}
