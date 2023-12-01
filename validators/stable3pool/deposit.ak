use aiken/list
use aiken/transaction.{ScriptContext, Spend, find_input}
use aiken/transaction/credential.{ScriptCredential, VerificationKeyCredential}
use aiken/transaction/value.{quantity_of}
use whalepools/stable3pool/types.{DepositData, PoolData}
use whalepools/stable3pool/utils.{convert_deposit_datum, convert_pool_datum}

validator {
  fn deposit(datum: DepositData, ctx: ScriptContext) -> Bool {
    let ScriptContext { transaction, purpose } = ctx
    expect Spend(spent_utxo_reference) = purpose
    // Input data:
    expect Some(self_input) =
      find_input(transaction.inputs, spent_utxo_reference)
    expect Some(pool_input) = list.at(transaction.inputs, 0)
    let pool0 = pool_input.output
    let self = self_input.output

    let pool0_datum: PoolData = convert_pool_datum(pool0.datum)

    let self_datum: DepositData = convert_deposit_datum(self.datum)

    // Output data:
    expect Some(pool1) = list.at(transaction.outputs, 0)
    expect Some(user_output) = list.at(transaction.outputs, 1)
    let pool1_datum: PoolData = convert_pool_datum(pool1.datum)

    // Validations:
    // 1. Deposited into the valid pool:
    let valid_pool =
      quantity_of(pool0.value, datum.pool_nft.policy, datum.pool_nft.name) == 1
    // 2. Valid expected tokens (and their amount in user's output):
    let received_lp_amount =
      quantity_of(
        user_output.value,
        pool0_datum.lp_token.policy,
        pool0_datum.lp_token.name,
      )
    let inv = pool0_datum.inv
    let inv_no_fees = pool1_datum.inv_no_fees

    let lp_amount =
      quantity_of(
        pool0.value,
        pool0_datum.lp_token.policy,
        pool0_datum.lp_token.name,
      )
    let supply_lp = 9223372036854775807 - lp_amount
    let delta_supply_lp = ( inv_no_fees - inv ) * supply_lp / inv

    let valid_lp_amount_received =
      received_lp_amount >= self_datum.min_expected_lp_amount && received_lp_amount >= delta_supply_lp

    // 3. Valid successor:
    let valid_successor =
      when user_output.address.payment_credential is {
        VerificationKeyCredential(successor_cred) ->
          self_datum.redeemer == successor_cred
        ScriptCredential(_) -> self.address == user_output.address
      }

    valid_pool && valid_lp_amount_received && valid_successor || list.has(
      ctx.transaction.extra_signatories,
      self_datum.redeemer,
    )
  }
}
