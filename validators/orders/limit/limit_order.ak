use aiken/builtin
use aiken/dict
use aiken/list
use aiken/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, ScriptContext, Spend,
  Transaction, TransactionId, WithdrawFrom, find_input,
}
use aiken/transaction/credential.{
  Address, Credential, Inline, Referenced, ScriptCredential, from_script,
  from_verification_key,
}
use aiken/transaction/value.{
  PolicyId, ada_asset_name, ada_policy_id, add, from_lovelace, quantity_of,
}
use splash/lovelace.{lovelace_of}
use splash/orders/types.{OrderType}
use splash/plutus.{Asset, VerificationKeyHash, mk_asset}
use splash/rational.{Rational, rational}

const this_tag = #[0]

type LimitOrderConfig {
  tag: OrderType,
  /// Beacon used to track progress.
  beacon: PolicyId,
  /// What we receive.
  input: Asset,
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
  /// Where the output from the order must go.
  redeemer_address: Address,
  /// PKH authorized to cancel order.
  cancellation_pkh: VerificationKeyHash,
  /// Executors permitted to execute this order.
  permitted_executors: List<VerificationKeyHash>,
}

validator(witness: Referenced<Credential>) {
  fn limit_order(
    conf: LimitOrderConfig,
    action: Bool,
    ctx: ScriptContext,
  ) -> Bool {
    let tx = ctx.transaction
    when action is {
      True -> dict.has_key(tx.withdrawals, witness)?
      False -> {
        let is_authorized =
          list.has(tx.extra_signatories, conf.cancellation_pkh)
        expect Spend(spent_utxo_reference) = ctx.purpose
        expect Some(self_input) = find_input(tx.inputs, spent_utxo_reference)
        let output = builtin.head_list(tx.outputs)
        let value_returned = self_input.output.value == output.value
        let correct_redeemer = output.address == conf.redeemer_address
        is_authorized && value_returned && correct_redeemer
      }
    }
  }
}

validator {
  fn batch_witness(_: Data, ctx: ScriptContext) -> Bool {
    let tx = ctx.transaction
    left_biased_zip_validate(
      tx.inputs,
      tx.outputs,
      tx.extra_signatories,
      True,
      validation_step,
    )?
  }
}

fn validation_step(
  input: Input,
  output: Output,
  signatories: List<VerificationKeyHash>,
) -> Bool {
  trace @"entered validator"
  let self = input.output
  expect InlineDatum(datum) = self.datum
  let (_, fields) = builtin.un_constr_data(datum)
  let tag: Data = builtin.head_list(fields)
  let this_tag: Data = this_tag
  if tag == this_tag {
    trace @"tag coincides"
    expect conf: LimitOrderConfig = datum
    trace @"datum parsed"
    let successor = output

    let self_value = self.value
    let successor_value = successor.value

    let input_policy = conf.input.policy
    let input_tn = conf.input.name
    let output_policy = conf.output.policy
    let output_tn = conf.output.name

    let ada_is_input = input_policy == ada_policy_id
    let ada_is_output = output_policy == ada_policy_id

    let input_0 = quantity_of(self_value, input_policy, input_tn)
    let output_0 = quantity_of(self_value, output_policy, output_tn)
    let input_1 = quantity_of(successor_value, input_policy, input_tn)
    let output_1 = quantity_of(successor_value, output_policy, output_tn)

    let lovelace_0 =
      if ada_is_input {
        input_0
      } else if ada_is_output {
        output_0
      } else {
        lovelace_of(self_value)
      }
    let lovelace_1 =
      if ada_is_input {
        input_1
      } else if ada_is_output {
        output_1
      } else {
        lovelace_of(successor_value)
      }

    let output_delta = output_1 - output_0
    let input_removed = input_0 - input_1
    let lovelace_delta = lovelace_1 - lovelace_0

    let cost_per_ex_step = conf.cost_per_ex_step
    let tradable_input_0 = conf.tradable_input
    let fee_0 = conf.fee
    let base_price = conf.base_price

    let self_address = self.address
    let successor_address = successor.address
    // Stake cred preserved.
    let valid_stake_cred =
      successor_address.stake_credential == self_address.stake_credential

    let is_terminated = successor_address == conf.redeemer_address
    let valid_execution =
      if is_terminated {
        trace @"entered is_terminated validation"
        let tradable_input_removed = tradable_input_0
        let fee_removed = fee_0
        let output_added =
          if ada_is_output {
            output_delta + fee_removed + cost_per_ex_step
          } else {
            output_delta
          }
        let valid_exchange_rate =
          output_added * base_price.denom >= tradable_input_removed * base_price.num
        let overall_integrity =
          if ada_is_input {
            input_removed <= tradable_input_removed + fee_removed + cost_per_ex_step
          } else {
            if ada_is_output {
              input_removed <= tradable_input_removed
            } else {
              input_removed <= tradable_input_removed && -lovelace_delta <= fee_removed + cost_per_ex_step
            }
          }
        valid_exchange_rate? && overall_integrity?
      } else {
        trace @"entered !is_terminated validation"
        expect InlineDatum(successor_datum) = successor.datum
        expect successor_conf: LimitOrderConfig = successor_datum
        trace @"successor.datum parsed"
        let tradable_input_1 = successor_conf.tradable_input
        let tradable_input_removed = tradable_input_0 - tradable_input_1
        let fee_1 = successor_conf.fee
        let fee_removed = fee_0 - fee_1
        let output_added =
          if ada_is_output {
            output_delta + fee_removed + cost_per_ex_step
          } else {
            output_delta
          }
        let valid_exchange_rate =
          output_added * base_price.denom >= tradable_input_removed * base_price.num
        let valid_fee =
          fee_removed <= tradable_input_removed * fee_0 / tradable_input_0
        let overall_integrity =
          if ada_is_input {
            input_removed <= tradable_input_removed + fee_removed + cost_per_ex_step
          } else {
            if ada_is_output {
              input_removed <= tradable_input_removed
            } else {
              input_removed <= tradable_input_removed && -lovelace_delta <= fee_removed + cost_per_ex_step
            }
          }
        // Execution step is worth it.
        let min_marginal_output = conf.min_marginal_output
        let valid_marginal_output = output_added >= min_marginal_output
        // Script preserved.
        let script_preserved = self_address == successor_address
        let expected_successor_conf =
          LimitOrderConfig {
            ..conf,
            tradable_input: tradable_input_1,
            fee: fee_1,
          }
        // Immutable field are unchanged.
        let config_preserved = successor_conf == expected_successor_conf
        valid_exchange_rate? && valid_fee? && overall_integrity? && valid_marginal_output? && script_preserved? && config_preserved?
      }

    let valid_authorization =
      when conf.permitted_executors is {
        [] -> True
        non_empty_executors -> {
          let executor_cred = builtin.head_list(signatories)
          list.has(non_empty_executors, executor_cred)
        }
      }

    valid_execution? && valid_stake_cred? && valid_authorization?
  } else {
    trace @"tags don't coincide"
    True
  }
}

fn left_biased_zip_validate(
  lhs: List<a>,
  rhs: List<b>,
  ctx: ctx,
  acc: Bool,
  predicate: fn(a, b, ctx) -> Bool,
) -> Bool {
  when lhs is {
    [] -> acc
    [x, ..xs] ->
      when rhs is {
        [] -> fail
        [y, ..ys] ->
          left_biased_zip_validate(
            xs,
            ys,
            ctx,
            acc && predicate(x, y, ctx),
            predicate,
          )
      }
  }
}

const test_redeemer = #"a2"

const test_order_validator_sh = #"a4"

const policy_y = #"f2"

const name_y = "a"

const beacon = "beacon"

fn make_order(
  tradable_input: Int,
  cost_per_ex_step: Int,
  max_ex_steps: Int,
  min_marginal_output: Int,
  fee: Int,
  base_price: Rational,
  redeemer_reward_pkh: VerificationKeyHash,
  permitted_executors: List<VerificationKeyHash>,
) -> (Output, LimitOrderConfig) {
  let conf =
    LimitOrderConfig {
      tag: this_tag,
      beacon,
      tradable_input,
      cost_per_ex_step,
      min_marginal_output,
      input: mk_asset(ada_policy_id, ada_asset_name),
      output: mk_asset(policy_y, name_y),
      base_price,
      fee,
      redeemer_address: from_verification_key(redeemer_reward_pkh),
      cancellation_pkh: redeemer_reward_pkh,
      permitted_executors,
    }
  let address = from_script(test_order_validator_sh)
  let lovelace_in = tradable_input + fee + cost_per_ex_step * max_ex_steps
  let value = from_lovelace(lovelace_in)
  let output =
    Output { address, value, datum: InlineDatum(conf), reference_script: None }
  (output, conf)
}

const hex32 = #"f4c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50"

const pkh1 = #"f686d9aa9f784c7e3dc0027c57505444c0383776d159733776226ed2"

const pkh2 = #"1544d2ed5fc4fafd503ef7334ccab11eb8a0815817aa0822938c4302"

const pkh3 = #"b1ac43b559d103921480b55b64e818be4af47fb9f70ee5e1bc19dfa7"

const test_witness = #"c4"

fn dummy_compare(_lhs: a, _rhs: a) -> Ordering {
  Equal
}

type Foo {
  a: Int,
  b: ByteArray,
}

test normal_partial_swap_ada_to_token() {
  let tradable_input0 = 1_000_000
  let fee0 = 5000
  let cost_per_ex_step = 10000
  let steps = 5
  let price = rational(1, 2)
  let tradable_input_removed = 200_000
  let tradable_input1 = tradable_input0 - tradable_input_removed
  let input_amount0 = tradable_input0 + fee0 + cost_per_ex_step * steps
  let fee_removed = fee0 * tradable_input_removed / input_amount0
  let fee1 = fee0 - fee_removed
  let input_removed = tradable_input_removed + fee_removed + cost_per_ex_step
  let output_amount0 = 10_000
  let input_amount1 = input_amount0 - input_removed
  let output_added = tradable_input_removed * price.num / price.denom
  let output_amount1 = output_amount0 + output_added
  test_partial_swap(
    input: mk_asset(ada_policy_id, ada_asset_name),
    output: mk_asset("x", "x"),
    input_amount: (input_amount0, input_amount1),
    output_amount: (output_amount0, output_amount1),
    lovelace_amount: (input_amount0, input_amount1),
    tradable_input: (tradable_input0, tradable_input1),
    fee: (fee0, fee1),
    cost_per_ex_step: cost_per_ex_step,
    min_marginal_output: 1000,
    base_price: price,
  )
}

test normal_partial_swap_token_to_ada() {
  let tradable_input0 = 1_000_000
  let fee0 = 5000
  let cost_per_ex_step = 10000
  let steps = 5
  let price = rational(1, 2)
  let tradable_input_removed = 200_000
  let tradable_input1 = tradable_input0 - tradable_input_removed
  let input_amount0 = tradable_input0
  let fee_removed = fee0 * tradable_input_removed / input_amount0
  let fee1 = fee0 - fee_removed
  let input_removed = tradable_input_removed
  let tradable_output0 = 10_000
  let output_amount0 = tradable_output0 + fee0 + cost_per_ex_step * steps
  let input_amount1 = input_amount0 - input_removed
  let output_added =
    tradable_input_removed * price.num / price.denom - fee_removed - cost_per_ex_step
  let output_amount1 = output_amount0 + output_added
  test_partial_swap(
    input: mk_asset("x", "x"),
    output: mk_asset(ada_policy_id, ada_asset_name),
    input_amount: (input_amount0, input_amount1),
    output_amount: (output_amount0, output_amount1),
    lovelace_amount: (output_amount0, output_amount1),
    tradable_input: (tradable_input0, tradable_input1),
    fee: (fee0, fee1),
    cost_per_ex_step: cost_per_ex_step,
    min_marginal_output: 1000,
    base_price: price,
  )
}

test normal_partial_swap_token_to_token() {
  let tradable_input0 = 1_000_000
  let fee0 = 5000
  let cost_per_ex_step = 10000
  let steps = 5
  let price = rational(1, 2)
  let tradable_input_removed = 200_000
  let tradable_input1 = tradable_input0 - tradable_input_removed
  let input_amount0 = tradable_input0
  let fee_removed = fee0 * tradable_input_removed / input_amount0
  let fee1 = fee0 - fee_removed
  let input_removed = tradable_input_removed
  let tradable_output0 = 10_000
  let output_amount0 = tradable_output0
  let lovelace0 = fee0 + cost_per_ex_step * steps
  let lovelace1 = lovelace0 - fee_removed - cost_per_ex_step
  let input_amount1 = input_amount0 - input_removed
  let output_added = tradable_input_removed * price.num / price.denom
  let output_amount1 = output_amount0 + output_added
  test_partial_swap(
    input: mk_asset("x", "x"),
    output: mk_asset("y", "y"),
    input_amount: (input_amount0, input_amount1),
    output_amount: (output_amount0, output_amount1),
    lovelace_amount: (lovelace0, lovelace1),
    tradable_input: (tradable_input0, tradable_input1),
    fee: (fee0, fee1),
    cost_per_ex_step: cost_per_ex_step,
    min_marginal_output: 1000,
    base_price: price,
  )
}

fn test_partial_swap(
  input: Asset,
  output: Asset,
  input_amount: (Int, Int),
  output_amount: (Int, Int),
  lovelace_amount: (Int, Int),
  tradable_input: (Int, Int),
  fee: (Int, Int),
  cost_per_ex_step: Int,
  min_marginal_output: Int,
  base_price: Rational,
) -> Bool {
  let conf0 =
    LimitOrderConfig {
      tag: this_tag,
      beacon: "beacon",
      tradable_input: tradable_input.1st,
      cost_per_ex_step,
      min_marginal_output,
      input,
      output,
      base_price,
      fee: fee.1st,
      redeemer_address: from_verification_key("redeemer"),
      cancellation_pkh: "canceller",
      permitted_executors: [],
    }
  let address = from_script("spending_script")
  let ada_is_input = input.policy == ada_policy_id
  let ada_is_output = output.policy == ada_policy_id
  let value0 =
    if ada_is_input {
      add(
        from_lovelace(lovelace_amount.1st),
        output.policy,
        output.name,
        output_amount.1st,
      )
    } else if ada_is_output {
      add(
        from_lovelace(lovelace_amount.1st),
        input.policy,
        input.name,
        input_amount.1st,
      )
    } else {
      add(
        add(
          from_lovelace(lovelace_amount.1st),
          output.policy,
          output.name,
          output_amount.1st,
        ),
        input.policy,
        input.name,
        input_amount.1st,
      )
    }
  let value1 =
    if ada_is_input {
      add(
        from_lovelace(lovelace_amount.2nd),
        output.policy,
        output.name,
        output_amount.2nd,
      )
    } else if ada_is_output {
      add(
        from_lovelace(lovelace_amount.2nd),
        input.policy,
        input.name,
        input_amount.2nd,
      )
    } else {
      add(
        add(
          from_lovelace(lovelace_amount.2nd),
          output.policy,
          output.name,
          output_amount.2nd,
        ),
        input.policy,
        input.name,
        input_amount.2nd,
      )
    }
  let output0 =
    Output {
      address,
      value: value0,
      datum: InlineDatum(conf0),
      reference_script: None,
    }
  let order_input_ref =
    OutputReference { transaction_id: TransactionId(hex32), output_index: 0 }
  let order_input = Input { output_reference: order_input_ref, output: output0 }
  let order_output =
    Output {
      address,
      value: value1,
      datum: InlineDatum(
        LimitOrderConfig {
          ..conf0,
          tradable_input: tradable_input.2nd,
          fee: fee.2nd,
        },
      ),
      reference_script: None,
    }
  let witness = Inline(ScriptCredential("batch_wit"))
  let spending_ctx =
    ScriptContext {
      purpose: Spend(order_input_ref),
      transaction: Transaction {
        ..transaction.placeholder(),
        inputs: [order_input],
        outputs: [order_output],
        withdrawals: dict.new()
          |> dict.insert(witness, 0, dummy_compare),
      },
    }
  let withdrawal_ctx =
    ScriptContext {
      purpose: WithdrawFrom(witness),
      transaction: Transaction {
        ..transaction.placeholder(),
        inputs: [order_input],
        outputs: [order_output],
      },
    }
  limit_order(witness, conf0, True, spending_ctx) && batch_witness(
    0,
    withdrawal_ctx,
  )
}

fn test_mixed_full_swap(
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
      test_redeemer,
      permitted_executors,
    )
  let order_input_ref =
    OutputReference { transaction_id: TransactionId(hex32), output_index: 0 }
  let order_input =
    Input { output_reference: order_input_ref, output: initial_order_utxo }
  let removed_input =
    removed_tradable_input + removed_ex_fee + removed_ex_budget
  let third_output =
    Output {
      address: from_script("fghjj"),
      value: from_lovelace(100),
      datum: InlineDatum(Foo { a: 1, b: "foo" }),
      reference_script: None,
    }
  let third_input =
    Input {
      output_reference: OutputReference {
        transaction_id: TransactionId(hex32),
        output_index: 1,
      },
      output: third_output,
    }
  let order_output =
    Output {
      address: initial_conf.redeemer_address,
      value: add(
        add(initial_order_utxo.value, policy_y, name_y, added_output),
        ada_policy_id,
        ada_asset_name,
        -removed_input,
      ),
      datum: NoDatum,
      reference_script: None,
    }
  let wit = Inline(ScriptCredential(test_witness))
  let ctx =
    ScriptContext {
      purpose: Spend(order_input_ref),
      transaction: transaction.placeholder()
        |> fn(transaction) {
             Transaction {
               ..transaction,
               inputs: [third_input, order_input],
               outputs: [third_output, order_output],
               extra_signatories: [executor],
               withdrawals: dict.new()
                 |> dict.insert(wit, 0, dummy_compare),
             }
           },
    }
  limit_order(wit, initial_conf, True, ctx) && batch_witness(0, ctx)
}

fn test_batched_full_swap(
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
  size: Int,
) -> Bool {
  let (initial_order_utxo, initial_conf) =
    make_order(
      tradable_input,
      cost_per_ex_step,
      max_ex_steps,
      min_marginal_output,
      fee,
      base_price,
      test_redeemer,
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
      address: initial_conf.redeemer_address,
      value: add(
        add(initial_order_utxo.value, policy_y, name_y, added_output),
        ada_policy_id,
        ada_asset_name,
        -removed_input,
      ),
      datum: NoDatum,
      reference_script: None,
    }
  let wit = Inline(ScriptCredential(test_witness))
  let ctx =
    ScriptContext {
      purpose: Spend(order_input_ref),
      transaction: transaction.placeholder()
        |> fn(transaction) {
             Transaction {
               ..transaction,
               inputs: list.repeat(order_input, size),
               outputs: list.repeat(order_output, size),
               extra_signatories: [executor],
               withdrawals: dict.new()
                 |> dict.insert(wit, 0, dummy_compare),
             }
           },
    }
  list.foldl(
    list.range(0, size),
    True,
    fn(_x, acc) { acc && limit_order(wit, initial_conf, True, ctx) },
  ) && batch_witness(0, ctx)
}

test valid_batched_full_swap_at_base_price() {
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
    []
  test_batched_full_swap(
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
    pkh1,
    46,
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
    [pkh1, pkh2]
  test_batched_full_swap(
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
    pkh1,
    1,
  )
}

test valid_mixed_full_swap_at_base_price() {
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
  test_mixed_full_swap(
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
    pkh1,
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
  !test_batched_full_swap(
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
    50,
  )
}
