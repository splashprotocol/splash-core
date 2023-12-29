//// Spot order.
//// Can be executed at a configured or better price as long as there is enough budget. 

use aiken/list
use aiken/transaction.{
  InlineDatum, Input, Output, OutputReference, ScriptContext, Spend, Transaction,
  TransactionId, find_input,
}
use aiken/transaction/credential.{
  ScriptCredential, VerificationKeyCredential, from_script,
}
use aiken/transaction/value.{
  ada_asset_name, ada_policy_id, add, from_lovelace, lovelace_of, quantity_of,
}
use whalepools/plutus.{Asset, VerificationKeyHash, mk_asset}
use whalepools/rational.{Rational, rational}

type ConfigNativeToToken {
  // What we receive.
  output: Asset,
  // Worst acceptable price (Output/Input).
  base_price: Rational,
  // How much we pay for each unit of ouput asset.
  fee_per_output: Rational,
  /// Amount of output sufficient for the order to be terminated.
  termination_threshold: Int,
  // Redeemer PKH.
  redeemer_reward_pkh: VerificationKeyHash,
  // Redeemer stake PKH.
  redeemer_stake_pkh: List<VerificationKeyHash>,
}

type Action {
  /// Execute order
  Exec { successor_ix: Int }
  Cancel
}

validator {
  fn spot_native_to_token(
    conf: ConfigNativeToToken,
    action: Action,
    ctx: ScriptContext,
  ) -> Bool {
    when action is {
      Exec { successor_ix } -> {
        let ScriptContext { transaction, purpose } = ctx
        expect Spend(spent_utxo_reference) = purpose
        expect Some(self_input) =
          find_input(transaction.inputs, spent_utxo_reference)

        let self = self_input.output
        expect Some(successor) = list.at(transaction.outputs, successor_ix)

        // Validate swap
        let output_policy = conf.output.policy
        let input_0 = lovelace_of(self.value)
        let output_0 = quantity_of(self.value, output_policy, conf.output.name)
        let input_1 = lovelace_of(successor.value)
        let output_1 =
          quantity_of(successor.value, output_policy, conf.output.name)

        let output_added = output_1 - output_0
        let fee_per_output = conf.fee_per_output
        let fee = output_added * fee_per_output.num / fee_per_output.denom
        let input_subtracted = input_0 - input_1 - fee
        // Correct exchange rate.
        let valid_execution =
          output_added * conf.base_price.denom >= input_subtracted * conf.base_price.num
        // No reverse trading.
        let valid_exchange = output_added > 0

        let valid_successor =
          when successor.address.payment_credential is {
            VerificationKeyCredential(successor_cred) -> {
              let order_satisfied = output_1 >= conf.termination_threshold
              let redeemer_correct = conf.redeemer_reward_pkh == successor_cred
              // Order is satisfied and accumulated funds are transferred to a right address.
              order_satisfied && redeemer_correct
            }
            ScriptCredential(_) -> {
              let script_preserved = self.address == successor.address
              let config_preserved = self.datum == successor.datum
              // Script and configuration are preserved.
              script_preserved && config_preserved
            }
          }

        valid_successor && valid_exchange && valid_execution
      }
      Cancel ->
        list.has(ctx.transaction.extra_signatories, conf.redeemer_reward_pkh)
    }
  }
}

const policy_x =
  #"0000000000000000000000000000000000000000000000000000000000000001"

const policy_y =
  #"0000000000000000000000000000000000000000000000000000000000000002"

fn swap_native_to_token(
  input_amount: Int,
  price: Rational,
  fee_per_output: Rational,
) -> (Output, ConfigNativeToToken) {
  let order_addr =
    from_script(#"f4c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50")
  let initial_order_value = from_lovelace(input_amount)
  let conf =
    ConfigNativeToToken {
      output: mk_asset(policy_y, "usd"),
      base_price: price,
      fee_per_output,
      termination_threshold: 1_000_000_000,
      redeemer_reward_pkh: #"f4c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50",
      redeemer_stake_pkh: [],
    }
  let order_in_out =
    Output {
      address: order_addr,
      value: initial_order_value,
      datum: InlineDatum(conf),
      reference_script: None,
    }
  let order_in_out_ref =
    OutputReference {
      transaction_id: TransactionId(
        #"f4c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50",
      ),
      output_index: 0,
    }
  let order_input =
    Input { output_reference: order_in_out_ref, output: order_in_out }
  let order_output =
    Output {
      address: order_addr,
      value: initial_order_value,
      datum: InlineDatum(conf),
      reference_script: None,
    }
  (order_in_out, conf)
}

fn test_partial_swap(
  order_input: Int,
  removed_input: Int,
  added_output: Int,
  price: Rational,
  fee: Rational,
) -> Bool {
  let (order_utxo_init, conf) = swap_native_to_token(order_input, price, fee)
  let order_input_ref =
    OutputReference {
      transaction_id: TransactionId(
        #"f4c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50",
      ),
      output_index: 0,
    }
  let order_input =
    Input { output_reference: order_input_ref, output: order_utxo_init }
  let updated_value =
    add(
      add(order_utxo_init.value, policy_y, conf.output.name, added_output),
      ada_policy_id,
      ada_asset_name,
      -removed_input,
    )
  let order_output =
    Output {
      address: order_utxo_init.address,
      value: updated_value,
      datum: InlineDatum(conf),
      reference_script: None,
    }
  let ctx =
    ScriptContext {
      purpose: Spend(order_input_ref),
      transaction: transaction.placeholder()
        |> fn(transaction) {
             Transaction {
               ..transaction,
               inputs: [order_input],
               outputs: [order_output],
             }
           },
    }
  let action = Exec { successor_ix: 0 }
  spot_native_to_token(conf, action, ctx)
}

test valid_partial_swap_at_exact_price() {
  let price = rational(61, 100)
  let fee = rational(1, 1_000_000)
  let input = 1_000_000_000
  let added_output = 100_000
  let removed_input =
    added_output * price.denom / price.num + added_output * fee.num / fee.denom
  test_partial_swap(input, removed_input, added_output, price, fee)
}

test invalid_partial_swap() {
  let price = rational(61, 100)
  let fee = rational(1, 1_000_000)
  let input = 1_000_000_000
  let added_output = 100_000
  let removed_input =
    added_output * price.denom / price.num + added_output * fee.num / fee.denom
  !test_partial_swap(input, removed_input + 1, added_output, price, fee)
}
