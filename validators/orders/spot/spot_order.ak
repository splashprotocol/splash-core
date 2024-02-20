use aiken/dict
use aiken/list
use aiken/builtin
use aiken/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, ScriptContext, Spend,
  Transaction, TransactionId,
}
use aiken/transaction/credential.{
  Address, Credential, Inline, Referenced, ScriptCredential,
  VerificationKeyCredential, from_script,
}
use aiken/transaction/value.{
  PolicyId, ada_asset_name, ada_policy_id, add, from_lovelace,
  quantity_of,
}
use splash/orders/spot.{beacon_name}
use splash/orders/types.{OrderConf}
use splash/plutus.{Asset, VerificationKeyHash, mk_asset}
use splash/rational.{Rational, rational}

const tpe = #[0]

type Action {
  Exec
  Cancel
}

type SpotOrderConfig {
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
}

validator(witness: Referenced<Credential>) {
  fn spot_order(
    conf: OrderConf<Data>,
    action: Action,
    ctx: ScriptContext,
  ) -> Bool {
    let tx = ctx.transaction
    when action is {
      Exec -> dict.has_key(ctx.transaction.withdrawals, witness)
      Cancel -> list.has(tx.extra_signatories, conf.redeemer_reward_pkh)
    }
  }
}

validator {
  fn batch_witness(_: Data, ctx: ScriptContext) -> Bool {
    let ScriptContext { transaction, .. } = ctx
    left_biased_zip_validate(
      transaction.inputs,
      transaction.outputs,
      transaction.extra_signatories,
      True,
      validation_step,
    )
  }
}

fn validation_step(
  input: Input,
  output: Output,
  signatories: List<VerificationKeyHash>,
) -> Bool {
  let self = input.output
  expect InlineDatum(datum) = self.datum
  expect annotated: OrderConf<Data> = datum
  if annotated.tpe == tpe {
    expect conf: SpotOrderConfig = annotated.extension
    let successor = output

    let self_value = self.value
    let successor_value = successor.value

    let input_policy = conf.input.policy
    let input_tn = conf.input.name
    let output_policy = conf.output.policy
    let output_tn = conf.output.name
    let input_0 = quantity_of(self_value, input_policy, input_tn)
    let output_0 = quantity_of(self_value, output_policy, output_tn)
    let input_1 = quantity_of(successor_value, input_policy, input_tn)
    let output_1 = quantity_of(successor_value, output_policy, output_tn)

    let output_added = output_1 - output_0
    let input_removed = input_0 - input_1

    let valid_exchange_direction = output_added > 0

    let cost_per_ex_step = conf.cost_per_ex_step
    let tradable_input_0 = conf.tradable_input
    let fee_0 = conf.fee
    let base_price = conf.base_price

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
          let redeemer_correct = annotated.redeemer_reward_pkh == successor_cred
          let valid_exchange = validate_exchange(conf.tradable_input, conf.fee)
          // Order is satisfied or exhausted and accumulated funds are transferred to the right address.
          redeemer_correct? && valid_exchange?
        }
        ScriptCredential(_) -> {
          // Validate exchange.
          expect InlineDatum(successor_datum) = successor.datum
          expect successor_conf: SpotOrderConfig = successor_datum
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
          let expected_successor_conf =
            OrderConf {
              ..annotated,
              extension: SpotOrderConfig {
                ..conf,
                tradable_input: tradable_input_1,
              },
            }
          // Immutable field are unchanged.
          let config_preserved = annotated == expected_successor_conf
          let remaining_ex_budget = input_1 - tradable_input_1
          let order_exhausted_or_satisfied =
            remaining_ex_budget < cost_per_ex_step || tradable_input_1 == 0
          !order_exhausted_or_satisfied && valid_marginal_output && beacon_preserved && script_preserved && config_preserved && valid_exchange
        }
      }

    let valid_authorization =
      when annotated.permitted_executors is {
        [] -> True
        non_empty_executors -> {
          let executor_cred = builtin.head_list(signatories)
          list.has(non_empty_executors, executor_cred)
        }
      }

    valid_successor && valid_stake_cred && valid_exchange_direction && valid_authorization
  } else {
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

const test_beacon_policy = #"a8"

const policy_x = #"f4"

const name_x = "b"

const policy_y = #"f2"

const name_y = "a"

fn make_order(
  tradable_input: Int,
  cost_per_ex_step: Int,
  max_ex_steps: Int,
  min_marginal_output: Int,
  fee: Int,
  base_price: Rational,
  redeemer_reward_pkh: VerificationKeyHash,
  permitted_executors: List<VerificationKeyHash>,
) -> (Output, OrderConf<SpotOrderConfig>) {
  let conf =
    OrderConf {
      tpe,
      redeemer_reward_pkh,
      permitted_executors,
      extension: SpotOrderConfig {
        beacon: test_beacon_policy,
        tradable_input,
        cost_per_ex_step,
        min_marginal_output,
        input: mk_asset(policy_x, name_x),
        output: mk_asset(policy_y, name_y),
        base_price,
        fee,
      },
    }
  let address = from_script(test_order_validator_sh)
  let lovelace_in = tradable_input + fee + cost_per_ex_step * max_ex_steps
  let value =
    add(from_lovelace(lovelace_in), test_beacon_policy, beacon_name, 1)
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
  let ext: Data = initial_conf.extension
  let opaque_iconf: OrderConf<Data> =
    OrderConf {
      tpe: initial_conf.tpe,
      redeemer_reward_pkh: initial_conf.redeemer_reward_pkh,
      permitted_executors: initial_conf.permitted_executors,
      extension: ext,
    }
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
    fn(_x, acc) { acc && spot_order(wit, opaque_iconf, Exec, ctx) },
  ) && batch_witness(0, ctx)
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
    [pkh1]
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
    91,
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
    91
  )
}
