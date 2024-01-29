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
  /// How much fee we pay to executor for whole swap.
  fee: Int,
  /// Redeemer PKH.
  redeemer_reward_pkh: VerificationKeyHash,
  /// Executors permitted to execute this order.
  permitted_executors: List<VerificationKeyHash>,
}

validator {
  fn spot_native_to_token(
    conf: ConfigNativeToToken,
    action: Action,
    ctx: ScriptContext,
  ) -> Bool {
    let signatories = ctx.transaction.extra_signatories
    when action is {
      Exec { successor_ix } -> {
        let ScriptContext { transaction, purpose } = ctx
        expect Spend(spent_utxo_reference) = purpose
        expect Some(self_input) =
          find_input(transaction.inputs, spent_utxo_reference)

        let self = self_input.output
        expect Some(successor) = list.at(transaction.outputs, successor_ix)

        let output_policy = conf.output.policy
        let input_0 = lovelace_of(self.value)
        let output_0 = quantity_of(self.value, output_policy, conf.output.name)
        let input_1 = lovelace_of(successor.value)
        let output_1 =
          quantity_of(successor.value, output_policy, conf.output.name)

        let output_added = output_1 - output_0
        let input_removed = input_0 - input_1

        let valid_exchange_direction = output_added > 0

        let cost_per_ex_step = conf.cost_per_ex_step
        let tradable_input_0 = conf.tradable_input
        let fee_0 = conf.fee
        let base_price = conf.base_price
        let permitted_executors = conf.permitted_executors

        let validate_exchange =
          fn(tradable_input_removed, fee_removed) {
            let valid_exchange_rate =
              output_added * base_price.denom >= tradable_input_removed * base_price.num
            let valid_fee =
              fee_removed <= tradable_input_removed * fee_0 / tradable_input_0
            let valid_input_delta_total =
              input_removed <= tradable_input_removed + fee_removed + cost_per_ex_step
            valid_exchange_rate && valid_fee && valid_input_delta_total
          }

        // Stake cred preserved.
        let valid_stake_cred =
          successor.address.stake_credential == self.address.stake_credential
        let valid_successor =
          when successor.address.payment_credential is {
            VerificationKeyCredential(successor_cred) -> {
              let redeemer_correct = conf.redeemer_reward_pkh == successor_cred
              let valid_exchange =
                validate_exchange(conf.tradable_input, conf.fee)
              // Order is satisfied or exhausted and accumulated funds are transferred to the right address.
              redeemer_correct? && valid_exchange?
            }
            ScriptCredential(_) -> {
              // Validate exchange.
              expect InlineDatum(successor_datum) = successor.datum
              expect successor_conf: ConfigNativeToToken = successor_datum
              let tradable_input_1 = successor_conf.tradable_input
              let tradable_input_removed = tradable_input_0 - tradable_input_1
              let fee_removed = fee_0 - successor_conf.fee
              let valid_exchange =
                validate_exchange(tradable_input_removed, fee_removed)
              // Execution step is worth it.
              let min_marginal_output = conf.min_marginal_output
              let valid_marginal_output = output_added >= min_marginal_output
              let beacon_preserved =
                quantity_of(successor.value, conf.beacon, beacon_name) == 1
              // Script preserved.
              let script_preserved = self.address == successor.address
              // Immutable field are unchanged.
              let config_preserved =
                conf.beacon == successor_conf.beacon && conf.output == successor_conf.output && base_price == successor_conf.base_price && cost_per_ex_step == successor_conf.cost_per_ex_step && min_marginal_output == successor_conf.min_marginal_output && conf.redeemer_reward_pkh == successor_conf.redeemer_reward_pkh && permitted_executors == successor_conf.permitted_executors
              let remaining_ex_budget = input_1 - tradable_input_1
              let order_exhausted_or_satisfied =
                remaining_ex_budget < cost_per_ex_step || tradable_input_1 == 0
              !order_exhausted_or_satisfied && valid_marginal_output && beacon_preserved && script_preserved && config_preserved && valid_exchange
            }
          }

        let valid_authorization =
          when permitted_executors is {
            [] -> True
            non_empty_executors ->
              list.any(
                non_empty_executors,
                fn(pkh) { list.has(signatories, pkh) },
              )
          }

        valid_successor && valid_stake_cred && valid_exchange_direction && valid_authorization
      }
      Cancel -> list.has(signatories, conf.redeemer_reward_pkh)
    }
  }
}

const policy_y =
  #"0000000000000000000000000000000000000000000000000000000000000002"

const name_y = "usd"

const hex32 = #"f4c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50"

const pkh1 = #"f686d9aa9f784c7e3dc0027c57505444c0383776d159733776226ed2"

const pkh2 = #"1544d2ed5fc4fafd503ef7334ccab11eb8a0815817aa0822938c4302"

const pkh3 = #"b1ac43b559d103921480b55b64e818be4af47fb9f70ee5e1bc19dfa7"

fn make_order(
  tradable_input: Int,
  cost_per_ex_step: Int,
  max_ex_steps: Int,
  min_marginal_output: Int,
  fee: Int,
  base_price: Rational,
  permitted_executors: List<VerificationKeyHash>,
) -> (Output, ConfigNativeToToken) {
  let conf =
    ConfigNativeToToken {
      beacon: hex32,
      tradable_input,
      cost_per_ex_step,
      min_marginal_output,
      output: mk_asset(policy_y, name_y),
      base_price,
      fee,
      redeemer_reward_pkh: hex32,
      permitted_executors,
    }
  let address = from_script(hex32)
  let lovelace_in = tradable_input + fee + cost_per_ex_step * max_ex_steps
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
  fee: Int,
  base_price: Rational,
  removed_tradable_input: Int,
  removed_ex_fee: Int,
  removed_ex_budget: Int,
  added_output: Int,
  permitted_executors: List<VerificationKeyHash>,
  executor: VerificationKeyHash,
) -> Bool {
  let (initial_order_utxo, initial_conf) =
    make_order(
      tradable_input,
      cost_per_ex_step,
      max_ex_steps,
      min_marginal_output,
      fee,
      base_price,
      permitted_executors,
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
               extra_signatories: [executor],
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
  fee: Int,
  base_price: Rational,
  removed_tradable_input: Int,
  removed_ex_fee: Int,
  removed_ex_budget: Int,
  added_output: Int,
  permitted_executors: List<VerificationKeyHash>,
  executor: VerificationKeyHash,
) -> Bool {
  let (initial_order_utxo, initial_conf) =
    make_order(
      tradable_input,
      cost_per_ex_step,
      max_ex_steps,
      min_marginal_output,
      fee,
      base_price,
      permitted_executors,
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
                 fee: fee - removed_ex_fee,
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
               extra_signatories: [executor],
             }
           },
    }
  let action = Exec { successor_ix: 0 }
  spot_native_to_token(initial_conf, action, ctx)
}

test valid_partial_swap_at_base_price() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let fee = 1_000_000
  let cost_per_ex_step = 30_000_000
  let max_ex_steps = 5
  let min_final_output = tradable_input * base_price.num / base_price.denom
  let min_marginal_output = min_final_output / max_ex_steps

  let removed_tradable_input = 600_000_000
  let added_output = removed_tradable_input * base_price.num / base_price.denom
  let removed_ex_fee = fee * removed_tradable_input / tradable_input
  let allowed_executors =
    [pkh1, pkh2, pkh3]
  test_partial_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee,
    base_price,
    removed_tradable_input,
    removed_ex_fee,
    cost_per_ex_step,
    added_output,
    allowed_executors,
    pkh3,
  )
}

test valid_open_partial_swap_at_base_price() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let fee = 1_000_000
  let cost_per_ex_step = 30_000_000
  let max_ex_steps = 5
  let min_final_output = tradable_input * base_price.num / base_price.denom
  let min_marginal_output = min_final_output / max_ex_steps

  let removed_tradable_input = 600_000_000
  let added_output = removed_tradable_input * base_price.num / base_price.denom
  let removed_ex_fee = fee * removed_tradable_input / tradable_input
  let allowed_executors =
    []
  test_partial_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee,
    base_price,
    removed_tradable_input,
    removed_ex_fee,
    cost_per_ex_step,
    added_output,
    allowed_executors,
    pkh3,
  )
}

test invalid_partial_swap_bad_marginal_output() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let fee = 1_000_000
  let cost_per_ex_step = 30_000_000
  let max_ex_steps = 5
  let min_final_output = tradable_input * base_price.num / base_price.denom
  let min_marginal_output = min_final_output / max_ex_steps

  let removed_tradable_input =
    min_marginal_output * base_price.denom / base_price.num - 1
  let added_output = removed_tradable_input * base_price.num / base_price.denom
  let removed_ex_fee = fee * removed_tradable_input / tradable_input
  let allowed_executors =
    [pkh1, pkh2, pkh3]
  !test_partial_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee,
    base_price,
    removed_tradable_input,
    removed_ex_fee,
    cost_per_ex_step,
    added_output,
    allowed_executors,
    pkh3,
  )
}

test invalid_partial_swap_exhausted_but_not_terminated() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let fee = 1_000_000
  let cost_per_ex_step = 30_000_000
  let max_ex_steps = 5
  let min_final_output = tradable_input * base_price.num / base_price.denom
  let min_marginal_output = min_final_output / max_ex_steps

  let removed_tradable_input = tradable_input
  let added_output = removed_tradable_input * base_price.num / base_price.denom
  let removed_ex_fee = fee * removed_tradable_input / tradable_input
  let allowed_executors =
    [pkh1, pkh2, pkh3]
  !test_partial_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee,
    base_price,
    removed_tradable_input,
    removed_ex_fee,
    cost_per_ex_step,
    added_output,
    allowed_executors,
    pkh3,
  )
}

test invalid_partial_swap_bad_executor() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let fee = 1_000_000
  let cost_per_ex_step = 30_000_000
  let max_ex_steps = 5
  let min_final_output = tradable_input * base_price.num / base_price.denom
  let min_marginal_output = min_final_output / max_ex_steps

  let removed_tradable_input =
    min_marginal_output * base_price.denom / base_price.num
  let added_output = removed_tradable_input * base_price.num / base_price.denom
  let removed_ex_fee = fee * removed_tradable_input / tradable_input
  let allowed_executors =
    [pkh1, pkh2]
  !test_partial_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee,
    base_price,
    removed_tradable_input,
    removed_ex_fee,
    cost_per_ex_step,
    added_output,
    allowed_executors,
    pkh3,
  )
}

test valid_full_swap_at_base_price() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let fee = 1_000_000
  let cost_per_ex_step = 30_000_000
  let max_ex_steps = 5
  let min_final_output = tradable_input * base_price.num / base_price.denom
  let min_marginal_output = min_final_output / max_ex_steps

  let removed_tradable_input = tradable_input
  let added_output = removed_tradable_input * base_price.num / base_price.denom
  let removed_ex_fee = fee * removed_tradable_input / tradable_input
  let allowed_executors =
    [pkh1, pkh2, pkh3]
  test_full_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee,
    base_price,
    removed_tradable_input,
    removed_ex_fee,
    cost_per_ex_step,
    added_output,
    allowed_executors,
    pkh3,
  )
}

test invalid_full_swap_bad_exchange_rate() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let fee = 1_000_000
  let cost_per_ex_step = 30_000_000
  let max_ex_steps = 5
  let min_final_output = tradable_input * base_price.num / base_price.denom
  let min_marginal_output = min_final_output / max_ex_steps

  let removed_tradable_input = tradable_input
  let added_output =
    removed_tradable_input * base_price.num / base_price.denom - 1
  let removed_ex_fee = fee * removed_tradable_input / tradable_input
  let allowed_executors =
    [pkh1, pkh2, pkh3]
  !test_full_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee,
    base_price,
    removed_tradable_input,
    removed_ex_fee,
    cost_per_ex_step,
    added_output,
    allowed_executors,
    pkh3,
  )
}

test invalid_full_swap_bad_exchange_direction() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let fee = 1_000_000
  let cost_per_ex_step = 30_000_000
  let max_ex_steps = 5
  let min_final_output = tradable_input * base_price.num / base_price.denom
  let min_marginal_output = min_final_output / max_ex_steps

  let removed_tradable_input = tradable_input
  let added_output = removed_tradable_input * base_price.num / base_price.denom
  let removed_ex_fee = fee * removed_tradable_input / tradable_input
  let allowed_executors =
    [pkh1, pkh2, pkh3]
  !test_full_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee,
    base_price,
    -removed_tradable_input,
    removed_ex_fee,
    cost_per_ex_step,
    -added_output,
    allowed_executors,
    pkh3,
  )
}

test invalid_full_swap_early_termination() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let fee = 1_000_000
  let cost_per_ex_step = 30_000_000
  let max_ex_steps = 5
  let min_final_output = tradable_input * base_price.num / base_price.denom
  let min_marginal_output = min_final_output / max_ex_steps

  let removed_tradable_input = tradable_input - 1
  let added_output = removed_tradable_input * base_price.num / base_price.denom
  let removed_ex_fee = fee * removed_tradable_input / tradable_input
  let allowed_executors =
    [pkh1, pkh2, pkh3]
  !test_full_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee,
    base_price,
    removed_tradable_input,
    removed_ex_fee,
    cost_per_ex_step,
    added_output,
    allowed_executors,
    pkh3,
  )
}

test invalid_full_swap_bad_executor() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let fee = 1_000_000
  let cost_per_ex_step = 30_000_000
  let max_ex_steps = 5
  let min_final_output = tradable_input * base_price.num / base_price.denom
  let min_marginal_output = min_final_output / max_ex_steps

  let removed_tradable_input = tradable_input
  let added_output = removed_tradable_input * base_price.num / base_price.denom
  let removed_ex_fee = fee * removed_tradable_input / tradable_input
  let allowed_executors =
    [pkh1, pkh2]
  !test_full_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee,
    base_price,
    removed_tradable_input,
    removed_ex_fee,
    cost_per_ex_step,
    added_output,
    allowed_executors,
    pkh3,
  )
}

test invalid_partial_swap_bad_exchange_rate() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let fee = 1_000_000
  let cost_per_ex_step = 30_000_000
  let max_ex_steps = 5
  let min_final_output = tradable_input * base_price.num / base_price.denom
  let min_marginal_output = min_final_output / max_ex_steps

  let removed_tradable_input = 60_000_000
  let added_output =
    removed_tradable_input * base_price.num / base_price.denom - 1
  let removed_ex_fee = fee * removed_tradable_input / tradable_input
  let allowed_executors =
    [pkh1, pkh2, pkh3]
  !test_partial_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee,
    base_price,
    removed_tradable_input,
    removed_ex_fee,
    cost_per_ex_step,
    added_output,
    allowed_executors,
    pkh3,
  ) && !test_partial_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee,
    base_price,
    removed_tradable_input + 1,
    removed_ex_fee,
    cost_per_ex_step,
    added_output,
    allowed_executors,
    pkh3,
  )
}

test invalid_partial_swap_bad_execution_budget() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let fee = 1_000_000
  let cost_per_ex_step = 30_000_000
  let max_ex_steps = 5
  let min_final_output = tradable_input * base_price.num / base_price.denom
  let min_marginal_output = min_final_output / max_ex_steps

  let removed_tradable_input = 60_000_000
  let added_output = removed_tradable_input * base_price.num / base_price.denom
  let removed_ex_fee = fee * removed_tradable_input / tradable_input
  let allowed_executors =
    [pkh1, pkh2, pkh3]
  !test_partial_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee,
    base_price,
    removed_tradable_input,
    removed_ex_fee,
    cost_per_ex_step + 1,
    added_output,
    allowed_executors,
    pkh3,
  )
}

test invalid_partial_swap_bad_execution_fee() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let fee = 1_000_000
  let cost_per_ex_step = 30_000_000
  let max_ex_steps = 5
  let min_final_output = tradable_input * base_price.num / base_price.denom
  let min_marginal_output = min_final_output / max_ex_steps

  let removed_tradable_input = 60_000_000
  let added_output = removed_tradable_input * base_price.num / base_price.denom
  let removed_ex_fee = fee * removed_tradable_input / tradable_input
  let allowed_executors =
    [pkh1, pkh2, pkh3]
  !test_partial_swap(
    tradable_input,
    cost_per_ex_step,
    max_ex_steps,
    min_marginal_output,
    fee,
    base_price,
    removed_tradable_input,
    removed_ex_fee + 1,
    cost_per_ex_step,
    added_output,
    allowed_executors,
    pkh3,
  )
}
