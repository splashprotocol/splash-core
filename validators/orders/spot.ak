//// Spot order.
//// Can be executed at a configured or better price as long as there is enough budget. 

use aiken/list
use aiken/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, ScriptContext, Spend,
  Transaction, TransactionId, find_input,
}
use aiken/transaction/credential.{
  Address, ScriptCredential, VerificationKeyCredential, from_script,
}
use aiken/transaction/value.{
  PolicyId, Value, ada_asset_name, ada_policy_id, add, from_lovelace,
  lovelace_of, quantity_of,
}
use whalepools/plutus.{Asset, VerificationKeyHash, mk_asset}
use whalepools/rational.{Rational, rational}

type ConfigNativeToToken {
  /// Beacon used to track progress.
  beacon: PolicyId,
  /// What we receive.
  output: Asset,
  /// Worst acceptable price (Output/Input).
  base_price: Rational,
  /// How much we pay for each unit of ouput asset.
  fee_per_output: Rational,
  /// Amount of output sufficient for the order to be terminated.
  termination_threshold: Int,
  /// Redeemer PKH.
  redeemer_reward_pkh: VerificationKeyHash,
  /// Redeemer stake PKH.
  redeemer_stake_pkh: List<VerificationKeyHash>,
}

type Action {
  /// Execute order
  Exec { successor_ix: Int }
  Cancel
}

const beacon_name = #"01"

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
              let order_satisfied_or_exhausted =
                output_1 >= conf.termination_threshold || input_1 == 0
              let redeemer_correct = conf.redeemer_reward_pkh == successor_cred
              // Order is satisfied or exhausted and accumulated funds are transferred to a right address.
              order_satisfied_or_exhausted && redeemer_correct
            }
            ScriptCredential(_) -> {
              let beacon_preserved =
                quantity_of(successor.value, conf.beacon, beacon_name) == 1
              let script_preserved = self.address == successor.address
              let config_preserved = self.datum == successor.datum
              // Script and configuration are preserved.
              beacon_preserved && script_preserved && config_preserved
            }
          }

        valid_successor && valid_exchange && valid_execution
      }
      Cancel ->
        list.has(ctx.transaction.extra_signatories, conf.redeemer_reward_pkh)
    }
  }
}

const policy_y =
  #"0000000000000000000000000000000000000000000000000000000000000002"

const name_y = "usd"

const beacon = #"f4c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50"

fn swap_native_to_token(
  input_value: Value,
  price: Rational,
  fee_per_output: Rational,
  termination_threshold: Int,
) -> (Output, ConfigNativeToToken) {
  let order_addr =
    from_script(#"f4c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50")
  let conf =
    ConfigNativeToToken {
      beacon,
      output: mk_asset(policy_y, name_y),
      base_price: price,
      fee_per_output,
      termination_threshold,
      redeemer_reward_pkh: #"f4c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50",
      redeemer_stake_pkh: [],
    }
  let order_out =
    Output {
      address: order_addr,
      value: input_value,
      datum: InlineDatum(conf),
      reference_script: None,
    }
  (order_out, conf)
}

fn test_atomic_swap(
  value_in: Value,
  value_out: Value,
  price: Rational,
  fee: Rational,
  termination_threshold: Int,
) -> Bool {
  let (order_utxo_init, conf) =
    swap_native_to_token(value_in, price, fee, termination_threshold)
  let order_input_ref =
    OutputReference {
      transaction_id: TransactionId(
        #"f4c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50",
      ),
      output_index: 0,
    }
  let order_input =
    Input { output_reference: order_input_ref, output: order_utxo_init }
  let order_output =
    Output {
      address: Address {
        payment_credential: VerificationKeyCredential(conf.redeemer_reward_pkh),
        stake_credential: None,
      },
      value: value_out,
      datum: NoDatum,
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

fn test_partial_swap(
  value_in: Value,
  value_out: Value,
  price: Rational,
  fee: Rational,
) -> Bool {
  let (order_utxo_init, conf) = swap_native_to_token(value_in, price, fee, 0)
  let order_input_ref =
    OutputReference {
      transaction_id: TransactionId(
        #"f4c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50",
      ),
      output_index: 0,
    }
  let order_input =
    Input { output_reference: order_input_ref, output: order_utxo_init }
  let order_output =
    Output {
      address: order_utxo_init.address,
      value: value_out,
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
  let value_in = add(from_lovelace(input), beacon, beacon_name, 1)
  let value_out =
    add(
      add(value_in, policy_y, name_y, added_output),
      ada_policy_id,
      ada_asset_name,
      -removed_input,
    )
  test_partial_swap(value_in, value_out, price, fee)
}

test invalid_partial_swap_bad_exchange_rate() {
  let price = rational(61, 100)
  let fee = rational(1, 1_000_000)
  let input = 1_000_000_000
  let added_output = 100_000
  let removed_input =
    added_output * price.denom / price.num + added_output * fee.num / fee.denom
  let value_in = add(from_lovelace(input), beacon, beacon_name, 1)
  let value_out =
    add(
      add(value_in, policy_y, name_y, added_output),
      ada_policy_id,
      ada_asset_name,
      -(removed_input + 1),
    )
  !test_partial_swap(value_in, value_out, price, fee)
}

test invalid_partial_swap_beacon_lost() {
  let price = rational(61, 100)
  let fee = rational(1, 1_000_000)
  let input = 1_000_000_000
  let added_output = 100_000
  let removed_input =
    added_output * price.denom / price.num + added_output * fee.num / fee.denom
  let value_in = add(from_lovelace(input), beacon, beacon_name, 1)
  let value_out =
    add(
      add(
        add(value_in, policy_y, name_y, added_output),
        ada_policy_id,
        ada_asset_name,
        -removed_input,
      ),
      beacon,
      beacon_name,
      1,
    )
  !test_partial_swap(value_in, value_out, price, fee)
}

test valid_atomic_swap_at_exact_price() {
  let price = rational(61, 100)
  let fee = rational(1, 1_000_000)
  let added_output = 1_000_000_000
  let removed_input =
    added_output * price.denom / price.num + added_output * fee.num / fee.denom
  let value_in = add(from_lovelace(removed_input), beacon, beacon_name, 1)
  let value_out =
    add(
      add(value_in, policy_y, name_y, added_output),
      ada_policy_id,
      ada_asset_name,
      -removed_input,
    )
  test_atomic_swap(value_in, value_out, price, fee, added_output)
}

test invalid_atomic_swap_invalid_exchange_rate() {
  let price = rational(61, 100)
  let fee = rational(1, 1_000_000)
  let added_output = 1_000_000_000
  let removed_input =
    added_output * price.denom / price.num + added_output * fee.num / fee.denom
  let value_in = add(from_lovelace(removed_input), beacon, beacon_name, 1)
  let value_out =
    add(
      add(value_in, policy_y, name_y, added_output - 1),
      ada_policy_id,
      ada_asset_name,
      -removed_input,
    )
  !test_atomic_swap(value_in, value_out, price, fee, added_output)
}

test invalid_atomic_swap_illigal_termination() {
  let price = rational(61, 100)
  let fee = rational(1, 1_000_000)
  let added_output = 1_000_000_000
  let removed_input =
    added_output * price.denom / price.num + added_output * fee.num / fee.denom
  let value_in = add(from_lovelace(removed_input), beacon, beacon_name, 1)
  let value_out =
    add(
      add(value_in, policy_y, name_y, added_output),
      ada_policy_id,
      ada_asset_name,
      -(removed_input - 1),
    )
  !test_atomic_swap(value_in, value_out, price, fee, added_output + 1)
}

test valid_atomic_swap_order_exhausted() {
  let price = rational(61, 100)
  let fee = rational(1, 1_000_000)
  let added_output = 1_000_000_000
  let removed_input =
    added_output * price.denom / price.num + added_output * fee.num / fee.denom
  let value_in = add(from_lovelace(removed_input), beacon, beacon_name, 1)
  let value_out =
    add(
      add(value_in, policy_y, name_y, added_output),
      ada_policy_id,
      ada_asset_name,
      -removed_input,
    )
  // Termination threshold is not reached, but the order is exhausted.
  test_atomic_swap(value_in, value_out, price, fee, added_output + 1)
}
