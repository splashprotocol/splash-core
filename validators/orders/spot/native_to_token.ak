use aiken/list
use aiken/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, ScriptContext, Spend,
  Transaction, TransactionId, find_input,
}
use aiken/transaction/credential.{
  Address, ScriptCredential, VerificationKeyCredential, from_script,
}
use aiken/transaction/value.{
  PolicyId, ada_asset_name, ada_policy_id, add, from_lovelace, lovelace_of,
  quantity_of,
}
use splash/orders/spot.{Action, Cancel, Exec, beacon_name}
use splash/plutus.{Asset, VerificationKeyHash, mk_asset}
use splash/rational.{Rational, rational}

type ConfigNativeToToken {
  /// Beacon used to track progress.
  beacon: PolicyId,
  /// Tradable amount of Lovelace.
  tradable_input: Int,
  /// Assumed cost (in Lovelace) of one step of execution.
  cost_per_ex_step: Int,
  /// Minimal marginal output allowed per execution step.
  min_marginal_output: Int,
  /// What we receive.
  output: Asset,
  /// Worst acceptable price (Output/Input).
  base_price: Rational,
  /// How much we pay for each unit of ouput asset.
  fee_per_output: Rational,
  /// Redeemer PKH.
  redeemer_reward_pkh: VerificationKeyHash,
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
        let cost_per_ex_step = conf.cost_per_ex_step
        let input_subtracted = input_0 - input_1 - fee - cost_per_ex_step
        // Correct exchange rate.
        let valid_exchange_rate =
          output_added * conf.base_price.denom >= input_subtracted * conf.base_price.num
        let remaining_tradable_input = conf.tradable_input - input_subtracted
        let remaining_ex_budget = input_1 - remaining_tradable_input
        // Only tradable input is used.
        let valid_input = remaining_tradable_input >= 0
        // No reverse trading.
        let valid_exchange_direction = output_added > 0

        let order_exhausted_or_satisfied =
          remaining_ex_budget < cost_per_ex_step || remaining_tradable_input == 0

        // Stake cred preserved.
        let valid_stake_cred =
          successor.address.stake_credential == self.address.stake_credential
        let valid_successor =
          when successor.address.payment_credential is {
            VerificationKeyCredential(successor_cred) -> {
              let redeemer_correct = conf.redeemer_reward_pkh == successor_cred
              // Order is satisfied or exhausted and accumulated funds are transferred to the right address.
              order_exhausted_or_satisfied && redeemer_correct
            }
            ScriptCredential(_) -> {
              // Execution step is worth it.
              let valid_marginal_output =
                output_added >= conf.min_marginal_output
              let beacon_preserved =
                quantity_of(successor.value, conf.beacon, beacon_name) == 1
              let script_preserved = self.address == successor.address
              expect InlineDatum(successor_datum) = successor.datum
              expect successor_conf: ConfigNativeToToken = successor_datum
              // Tradable input updated correctly.
              let valid_tradable_input =
                successor_conf.tradable_input == remaining_tradable_input
              // Immutable field are unchanged.
              let config_preserved =
                conf.beacon == successor_conf.beacon && conf.output == successor_conf.output && conf.base_price == successor_conf.base_price && conf.fee_per_output == successor_conf.fee_per_output && conf.cost_per_ex_step == successor_conf.cost_per_ex_step && conf.min_marginal_output == successor_conf.min_marginal_output && conf.redeemer_reward_pkh == successor_conf.redeemer_reward_pkh
              // Script and configuration are preserved.
              !order_exhausted_or_satisfied && valid_marginal_output && beacon_preserved && script_preserved && valid_tradable_input && config_preserved
            }
          }

        valid_successor && valid_stake_cred && valid_exchange_direction && valid_exchange_rate && valid_input
      }
      Cancel ->
        list.has(ctx.transaction.extra_signatories, conf.redeemer_reward_pkh)
    }
  }
}

const policy_y =
  #"0000000000000000000000000000000000000000000000000000000000000002"

const name_y = "usd"

const hex32 = #"f4c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50"

fn make_order(
  tradable_input: Int,
  cost_per_ex_step: Int,
  max_ex_steps: Int,
  min_marginal_output: Int,
  fee_per_output: Rational,
  base_price: Rational,
  best_price: Rational,
) -> (Output, ConfigNativeToToken) {
  let conf =
    ConfigNativeToToken {
      beacon: hex32,
      tradable_input,
      cost_per_ex_step,
      min_marginal_output,
      output: mk_asset(policy_y, name_y),
      base_price,
      fee_per_output,
      redeemer_reward_pkh: hex32,
    }
  let address = from_script(hex32)
  let reserved_fees =
    tradable_input * best_price.num * fee_per_output.denom / (
      best_price.denom * fee_per_output.num
    )
  let lovelace_in =
    tradable_input + reserved_fees + cost_per_ex_step * max_ex_steps
  let value = add(from_lovelace(lovelace_in), hex32, beacon_name, 1)
  let output =
    Output { address, value, datum: InlineDatum(conf), reference_script: None }
  (output, conf)
}

fn test_full_swap(
  tradable_input: Int,
  cost_per_ex_step: Int,
  max_ex_steps: Int,
  min_marginal_output: Int,
  fee_per_output: Rational,
  base_price: Rational,
  best_price: Rational,
  removed_tradable_input: Int,
  removed_ex_fee: Int,
  removed_ex_budget: Int,
  added_output: Int,
) -> Bool {
  let (initial_order_utxo, initial_conf) =
    make_order(
      tradable_input,
      cost_per_ex_step,
      max_ex_steps,
      min_marginal_output,
      fee_per_output,
      base_price,
      best_price,
    )
  let order_input_ref =
    OutputReference { transaction_id: TransactionId(hex32), output_index: 0 }
  let order_input =
    Input { output_reference: order_input_ref, output: initial_order_utxo }
  let removed_input =
    removed_tradable_input + removed_ex_fee + removed_ex_budget
  let order_output =
    Output {
      address: Address {
        payment_credential: VerificationKeyCredential(
          initial_conf.redeemer_reward_pkh,
        ),
        stake_credential: None,
      },
      value: add(
        add(initial_order_utxo.value, policy_y, name_y, added_output),
        ada_policy_id,
        ada_asset_name,
        -removed_input,
      ),
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
  spot_native_to_token(initial_conf, action, ctx)
}

fn test_partial_swap(
  tradable_input: Int,
  cost_per_ex_step: Int,
  max_ex_steps: Int,
  min_marginal_output: Int,
  fee_per_output: Rational,
  base_price: Rational,
  best_price: Rational,
  removed_tradable_input: Int,
  removed_ex_fee: Int,
  removed_ex_budget: Int,
  added_output: Int,
) -> Bool {
  let (initial_order_utxo, initial_conf) =
    make_order(
      tradable_input,
      cost_per_ex_step,
      max_ex_steps,
      min_marginal_output,
      fee_per_output,
      base_price,
      best_price,
    )
  let order_input_ref =
    OutputReference { transaction_id: TransactionId(hex32), output_index: 0 }
  let order_input =
    Input { output_reference: order_input_ref, output: initial_order_utxo }
  let removed_input =
    removed_tradable_input + removed_ex_fee + removed_ex_budget
  let order_output =
    Output {
      address: initial_order_utxo.address,
      value: add(
        add(initial_order_utxo.value, policy_y, name_y, added_output),
        ada_policy_id,
        ada_asset_name,
        -removed_input,
      ),
      datum: InlineDatum(
        initial_conf
          |> fn(conf) {
               ConfigNativeToToken {
                 ..conf,
                 tradable_input: tradable_input - removed_tradable_input,
               }
             },
      ),
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
  spot_native_to_token(initial_conf, action, ctx)
}

test valid_partial_swap_at_base_price() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let best_price = rational(70, 100)
  let fee_per_output = rational(1, 1_000)
  let cost_per_ex_step = 30_000_000
  let max_ex_steps = 5
  let min_final_output = tradable_input * base_price.num / base_price.denom
  let min_marginal_output = min_final_output / max_ex_steps

  let removed_tradable_input = 600_000_000
  let added_output = removed_tradable_input * base_price.num / base_price.denom
  let removed_ex_fee = added_output * fee_per_output.num / fee_per_output.denom
  test_partial_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee_per_output,
    base_price,
    best_price,
    removed_tradable_input,
    removed_ex_fee,
    cost_per_ex_step,
    added_output,
  )
}

test invalid_partial_swap_bad_marginal_output() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let best_price = rational(70, 100)
  let fee_per_output = rational(1, 1_000)
  let cost_per_ex_step = 30_000_000
  let max_ex_steps = 5
  let min_final_output = tradable_input * base_price.num / base_price.denom
  let min_marginal_output = min_final_output / max_ex_steps

  let removed_tradable_input =
    min_marginal_output * base_price.denom / base_price.num - 1
  let added_output = removed_tradable_input * base_price.num / base_price.denom
  let removed_ex_fee = added_output * fee_per_output.num / fee_per_output.denom
  !test_partial_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee_per_output,
    base_price,
    best_price,
    removed_tradable_input,
    removed_ex_fee,
    cost_per_ex_step,
    added_output,
  )
}

test valid_full_swap_at_base_price() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let best_price = rational(70, 100)
  let fee_per_output = rational(1, 1_000)
  let cost_per_ex_step = 30_000_000
  let max_ex_steps = 5
  let min_final_output = tradable_input * base_price.num / base_price.denom
  let min_marginal_output = min_final_output / max_ex_steps

  let removed_tradable_input = tradable_input
  let added_output = removed_tradable_input * base_price.num / base_price.denom
  let removed_ex_fee = added_output * fee_per_output.num / fee_per_output.denom
  test_full_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee_per_output,
    base_price,
    best_price,
    removed_tradable_input,
    removed_ex_fee,
    cost_per_ex_step,
    added_output,
  )
}

test valid_full_swap_at_best_price() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let best_price = rational(70, 100)
  let fee_per_output = rational(1, 1_000_000)
  let cost_per_ex_step = 30_000_000
  let max_ex_steps = 5
  let min_final_output = tradable_input * base_price.num / base_price.denom
  let min_marginal_output = min_final_output / max_ex_steps

  let removed_tradable_input = tradable_input
  let added_output = removed_tradable_input * best_price.num / best_price.denom
  let removed_ex_fee = added_output * fee_per_output.num / fee_per_output.denom
  test_full_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee_per_output,
    base_price,
    best_price,
    removed_tradable_input,
    removed_ex_fee,
    cost_per_ex_step,
    added_output,
  )
}

test invalid_full_swap_bad_exchange_rate() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let best_price = rational(70, 100)
  let fee_per_output = rational(1, 1_000_000)
  let cost_per_ex_step = 30_000_000
  let max_ex_steps = 5
  let min_final_output = tradable_input * base_price.num / base_price.denom
  let min_marginal_output = min_final_output / max_ex_steps

  let removed_tradable_input = tradable_input
  let added_output =
    removed_tradable_input * base_price.num / base_price.denom - 1
  let removed_ex_fee = added_output * fee_per_output.num / fee_per_output.denom
  !test_full_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee_per_output,
    base_price,
    best_price,
    removed_tradable_input,
    removed_ex_fee,
    cost_per_ex_step,
    added_output,
  )
}

test invalid_full_swap_bad_exchange_direction() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let best_price = rational(70, 100)
  let fee_per_output = rational(1, 1_000_000)
  let cost_per_ex_step = 30_000_000
  let max_ex_steps = 5
  let min_final_output = tradable_input * base_price.num / base_price.denom
  let min_marginal_output = min_final_output / max_ex_steps

  let removed_tradable_input = tradable_input
  let added_output = removed_tradable_input * base_price.num / base_price.denom
  let removed_ex_fee = added_output * fee_per_output.num / fee_per_output.denom
  !test_full_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee_per_output,
    base_price,
    best_price,
    -removed_tradable_input,
    removed_ex_fee,
    cost_per_ex_step,
    -added_output,
  )
}

test invalid_full_swap_early_termination() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let best_price = rational(70, 100)
  let fee_per_output = rational(1, 1_000_000)
  let cost_per_ex_step = 30_000_000
  let max_ex_steps = 5
  let min_final_output = tradable_input * base_price.num / base_price.denom
  let min_marginal_output = min_final_output / max_ex_steps

  let removed_tradable_input = tradable_input - 1
  let added_output = removed_tradable_input * base_price.num / base_price.denom
  let removed_ex_fee = added_output * fee_per_output.num / fee_per_output.denom
  !test_full_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee_per_output,
    base_price,
    best_price,
    removed_tradable_input,
    removed_ex_fee,
    cost_per_ex_step,
    added_output,
  )
}

test invalid_partial_swap_bad_exchange_rate() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let best_price = rational(70, 100)
  let fee_per_output = rational(1, 1_000_000)
  let cost_per_ex_step = 30_000_000
  let max_ex_steps = 5
  let min_final_output = tradable_input * base_price.num / base_price.denom
  let min_marginal_output = min_final_output / max_ex_steps

  let removed_tradable_input = 60_000_000
  let added_output =
    removed_tradable_input * base_price.num / base_price.denom - 1
  let removed_ex_fee = added_output * fee_per_output.num / fee_per_output.denom
  !test_partial_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee_per_output,
    base_price,
    best_price,
    removed_tradable_input,
    removed_ex_fee,
    cost_per_ex_step,
    added_output,
  ) && !test_partial_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee_per_output,
    base_price,
    best_price,
    removed_tradable_input + 1,
    removed_ex_fee,
    cost_per_ex_step,
    added_output,
  )
}

test invalid_partial_swap_bad_execution_budget() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let best_price = rational(70, 100)
  let fee_per_output = rational(1, 1_000_000)
  let cost_per_ex_step = 30_000_000
  let max_ex_steps = 5
  let min_final_output = tradable_input * base_price.num / base_price.denom
  let min_marginal_output = min_final_output / max_ex_steps

  let removed_tradable_input = 60_000_000
  let added_output = removed_tradable_input * base_price.num / base_price.denom
  let removed_ex_fee = added_output * fee_per_output.num / fee_per_output.denom
  !test_partial_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee_per_output,
    base_price,
    best_price,
    removed_tradable_input,
    removed_ex_fee,
    cost_per_ex_step + 1,
    added_output,
  )
}

test invalid_partial_swap_bad_execution_fee() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let best_price = rational(70, 100)
  let fee_per_output = rational(1, 1_000_000)
  let cost_per_ex_step = 30_000_000
  let max_ex_steps = 5
  let min_final_output = tradable_input * base_price.num / base_price.denom
  let min_marginal_output = min_final_output / max_ex_steps

  let removed_tradable_input = 60_000_000
  let added_output = removed_tradable_input * base_price.num / base_price.denom
  let removed_ex_fee = added_output * fee_per_output.num / fee_per_output.denom
  !test_partial_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee_per_output,
    base_price,
    best_price,
    removed_tradable_input,
    removed_ex_fee + 1,
    cost_per_ex_step,
    added_output,
  )
}
