use aiken/builtin
use aiken/dict
use aiken/interval.{Finite, Interval, IntervalBound, PositiveInfinity}
use aiken/list
use aiken/time.{PosixTime}
use aiken/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, ScriptContext, Spend,
  Transaction, TransactionId, ValidityRange, WithdrawFrom, find_input,
}
use aiken/transaction/credential.{
  Address, Credential, Inline, Referenced, ScriptCredential, from_script,
  from_verification_key, with_delegation_key,
}
use aiken/transaction/value.{
  PolicyId, Value, ada_asset_name, ada_policy_id, add, from_lovelace,
}
use splash/orders/types.{OrderType}
use splash/plutus.{Asset, VerificationKeyHash, mk_asset}
use splash/rational.{Rational, rational}
use splash/value_ext

const this_tag = #[1]

type InstantOrderConfig {
  tag: OrderType,
  /// Where the output from the order must go.
  redeemer_address: Address,
  /// What we receive.
  input: Asset,
  /// What we receive.
  output: Asset,
  /// Worst acceptable price (Output/Input).
  base_price: Rational,
  /// Flat fee for the whole swap.
  fee: Int,
  /// Lovelace amount that covers min utxo value.
  min_lovelace: Int,
  /// Executors permitted to execute this order.
  permitted_executors: VerificationKeyHash,
  /// When this order can be cancelled.
  cancellation_after: PosixTime,
  /// PKH authorized to cancel order.
  cancellation_pkh: VerificationKeyHash,
}

fn lower_bound(validity_range: ValidityRange) -> Int {
  expect IntervalBound { bound_type: Finite(lb), is_inclusive: True } =
    validity_range.lower_bound
  lb
}

validator(witness: Referenced<Credential>) {
  fn instant_order(
    conf: InstantOrderConfig,
    action: Bool,
    ctx: ScriptContext,
  ) -> Bool {
    let tx = ctx.transaction
    when action is {
      True ->
        (
          dict.to_list(tx.withdrawals)
            |> builtin.head_list
            |> builtin.fst_pair
        ) == witness
      False -> {
        let is_authorized =
          list.has(tx.extra_signatories, conf.cancellation_pkh)
        expect Spend(spent_utxo_reference) = ctx.purpose
        expect Some(self_input) = find_input(tx.inputs, spent_utxo_reference)
        let output = builtin.head_list(tx.outputs)
        let value_returned = self_input.output.value == output.value
        let correct_redeemer = output.address == conf.redeemer_address
        let tx_valid_after = lower_bound(tx.validity_range)
        let valid_cancellation = tx_valid_after >= conf.cancellation_after
        is_authorized && value_returned && correct_redeemer && valid_cancellation
      }
    }
  }
}

validator {
  fn batch_witness(_: Data, ctx: ScriptContext) -> Bool {
    expect WithdrawFrom(_) = ctx.purpose
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

const max_assets = 3

fn validation_step(
  input: Input,
  output: Output,
  signatories: List<VerificationKeyHash>,
) -> Bool {
  let self = input.output
  when self.datum is {
    InlineDatum(datum) -> {
      let (_, fields) = builtin.un_constr_data(datum)
      let tag = builtin.head_list(fields)
      let this_tag: Data = this_tag
      if tag == this_tag {
        expect conf: InstantOrderConfig = datum
        let successor = output

        let self_value = self.value
        let successor_value = successor.value

        let input = conf.input
        let output = conf.output
        let input_policy = input.policy
        let input_tn = input.name
        let output_policy = output.policy
        let output_tn = output.name

        let ada_is_input = input_policy == ada_policy_id
        let ada_is_output = output_policy == ada_policy_id

        expect [lovelace_pol_0, ..self_value_raw] =
          self_value |> value.to_dict |> dict.to_list
        let (_, lovelace_0) =
          lovelace_pol_0.2nd |> dict.to_list |> builtin.head_list
        expect [lovelace_pol_1, ..successor_value_raw] =
          successor_value |> value.to_dict |> dict.to_list
        let (_, lovelace_1) =
          lovelace_pol_1.2nd |> dict.to_list |> builtin.head_list

        let input_0 =
          if ada_is_input {
            lovelace_0
          } else {
            value_ext.quantity_of(
              self_value_raw,
              input_policy,
              input_tn,
              max_assets,
            )
          }
        let output_0 =
          if ada_is_output {
            lovelace_0
          } else {
            value_ext.quantity_of(
              self_value_raw,
              output_policy,
              output_tn,
              max_assets,
            )
          }

        let input_1 =
          if ada_is_input {
            lovelace_1
          } else {
            value_ext.quantity_of(
              successor_value_raw,
              input_policy,
              input_tn,
              max_assets,
            )
          }
        let output_1 =
          if ada_is_output {
            lovelace_1
          } else {
            value_ext.quantity_of(
              successor_value_raw,
              output_policy,
              output_tn,
              max_assets,
            )
          }

        let output_delta = output_1 - output_0

        let fee = conf.fee
        let tradable_input_0 =
          if ada_is_input {
            input_0 - fee - conf.min_lovelace
          } else {
            input_0
          }
        let base_price = conf.base_price

        let successor_address = successor.address
        let is_terminated = successor_address == conf.redeemer_address

        let tradable_input_1 =
          if ada_is_input {
            input_1 - conf.min_lovelace
          } else {
            input_1
          }
        let tradable_input_removed = tradable_input_0 - tradable_input_1
        let fee_removed = fee
        let output_added =
          if ada_is_output {
            output_delta + fee_removed
          } else {
            output_delta
          }
        let valid_exchange_rate =
          output_added * base_price.denom >= tradable_input_removed * base_price.num

        let valid_authorization =
          conf.permitted_executors == builtin.head_list(signatories)

        valid_exchange_rate? && is_terminated? && valid_authorization?
      } else {
        True
      }
    }
    _ -> True
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
        [] -> False
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

const min_lovelace = 1_500_000

fn after(lower_bound: a) -> Interval<a> {
  Interval {
    lower_bound: IntervalBound {
      bound_type: Finite(lower_bound),
      is_inclusive: True,
    },
    upper_bound: IntervalBound {
      bound_type: PositiveInfinity,
      is_inclusive: False,
    },
  }
}

fn reproduce_eol_order(address: Address, value: Value) -> Output {
  Output { address, value, datum: NoDatum, reference_script: None }
}

fn reproduce_order(
  script: ByteArray,
  value: Value,
  input: (PolicyId, ByteArray),
  output: (PolicyId, ByteArray),
  fee: Int,
  base_price: Rational,
  redeemer_address: Address,
  cancellation_pkh: VerificationKeyHash,
  permitted_executors: VerificationKeyHash,
) -> (Output, InstantOrderConfig) {
  let conf =
    InstantOrderConfig {
      tag: this_tag,
      input: mk_asset(input.1st, input.2nd),
      output: mk_asset(output.1st, output.2nd),
      base_price,
      fee,
      min_lovelace,
      redeemer_address,
      cancellation_pkh,
      permitted_executors,
      cancellation_after: 0,
    }
  let address = from_script(script)
  let output =
    Output { address, value, datum: InlineDatum(conf), reference_script: None }
  (output, conf)
}

test reproduce_preprod_execution_one_to_one() {
  let (ask, _) =
    reproduce_order(
      script: #"3e428b6289aba5280f1df5c1b9e0f55c28925f659e954858817630b6",
      value: from_lovelace(102_500_000),
      input: (ada_policy_id, ada_asset_name),
      output: (
        #"fd10da3e6a578708c877e14b6aaeda8dc3a36f666a346eec52a30b3a",
        #"74657374746f6b656e",
      ),
      fee: 500_000,
      base_price: Rational { num: 1, denom: 1000 },
      redeemer_address: from_verification_key(
        #"4be4fa25f029d14c0d723af4a1e6fa7133fc3a610f880336ad685cba",
      )
        |> with_delegation_key(
            #"5bda73043d43ad8df5ce75639cf48e1f2b4545403be92f0113e37537",
          ),
      cancellation_pkh: #"4be4fa25f029d14c0d723af4a1e6fa7133fc3a610f880336ad685cba",
      permitted_executors: pkh1,
    )
  let ask_input_ref =
    OutputReference {
      transaction_id: TransactionId(
        #"1428011538d509b0905860e874cc4efcc63df4fa5193b0d90e607f35c4a38cb2",
      ),
      output_index: 0,
    }
  let ask_input = Input { output_reference: ask_input_ref, output: ask }
  let (bid, _) =
    reproduce_order(
      script: #"3e428b6289aba5280f1df5c1b9e0f55c28925f659e954858817630b6",
      value: from_lovelace(2_500_000)
        |> add(
            #"fd10da3e6a578708c877e14b6aaeda8dc3a36f666a346eec52a30b3a",
            #"74657374746f6b656e",
            100_000,
          ),
      input: (
        #"fd10da3e6a578708c877e14b6aaeda8dc3a36f666a346eec52a30b3a",
        #"74657374746f6b656e",
      ),
      output: (ada_policy_id, ada_asset_name),
      fee: 500_000,
      base_price: Rational { num: 1000, denom: 1 },
      redeemer_address: from_verification_key(
        #"4be4fa25f029d14c0d723af4a1e6fa7133fc3a610f880336ad685cba",
      )
        |> with_delegation_key(
            #"5bda73043d43ad8df5ce75639cf48e1f2b4545403be92f0113e37537",
          ),
      cancellation_pkh: #"4be4fa25f029d14c0d723af4a1e6fa7133fc3a610f880336ad685cba",
      permitted_executors: pkh1,
    )
  let bid_input_ref =
    OutputReference {
      transaction_id: TransactionId(
        #"72193c6ceb5d4175b383d6326675563d7733fc9b7230f241eba7e2d9649012ec",
      ),
      output_index: 0,
    }
  let bid_input = Input { output_reference: bid_input_ref, output: bid }
  let ask_out =
    reproduce_eol_order(
      address: from_verification_key(
        #"4be4fa25f029d14c0d723af4a1e6fa7133fc3a610f880336ad685cba",
      )
        |> with_delegation_key(
            #"5bda73043d43ad8df5ce75639cf48e1f2b4545403be92f0113e37537",
          ),
      value: from_lovelace(2_000_000)
        |> add(
            #"fd10da3e6a578708c877e14b6aaeda8dc3a36f666a346eec52a30b3a",
            #"74657374746f6b656e",
            100_000,
          ),
    )
  let bid_out =
    reproduce_eol_order(
      address: from_verification_key(
        #"4be4fa25f029d14c0d723af4a1e6fa7133fc3a610f880336ad685cba",
      )
        |> with_delegation_key(
            #"5bda73043d43ad8df5ce75639cf48e1f2b4545403be92f0113e37537",
          ),
      value: from_lovelace(102_000_000),
    )
  let witness =
    Inline(
      ScriptCredential(
        #"339129f739e2905f3c3ea7727d6ee62b5057618144bf5d07b0fba7bd",
      ),
    )
  let withdrawal_ctx =
    ScriptContext {
      purpose: WithdrawFrom(witness),
      transaction: Transaction {
        ..transaction.placeholder(),
        inputs: [ask_input, bid_input],
        outputs: [ask_out, bid_out],
        extra_signatories: [pkh1],
      },
    }
  batch_witness(0, withdrawal_ctx)
}

fn make_order(
  tradable_input: Int,
  fee: Int,
  base_price: Rational,
  redeemer_reward_pkh: VerificationKeyHash,
  permitted_executors: VerificationKeyHash,
) -> (Output, InstantOrderConfig) {
  let conf =
    InstantOrderConfig {
      tag: this_tag,
      input: mk_asset(ada_policy_id, ada_asset_name),
      output: mk_asset(policy_y, name_y),
      base_price,
      fee,
      min_lovelace,
      redeemer_address: from_verification_key(redeemer_reward_pkh),
      cancellation_pkh: redeemer_reward_pkh,
      permitted_executors,
      cancellation_after: 0,
    }
  let address = from_script(test_order_validator_sh)
  let lovelace_in = tradable_input + fee + min_lovelace
  let value = from_lovelace(lovelace_in)
  let output =
    Output { address, value, datum: InlineDatum(conf), reference_script: None }
  (output, conf)
}

const hex32 = #"f4c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50"

const pkh1 = #"f686d9aa9f784c7e3dc0027c57505444c0383776d159733776226ed2"

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
  let steps = 5
  let price = rational(1, 2)
  let tradable_input_removed = 200_000
  let input_amount0 = tradable_input0 + fee0 * steps
  let fee_removed = fee0 * tradable_input_removed / input_amount0
  let input_removed = tradable_input_removed + fee_removed
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
    fee: fee0,
    base_price: price,
  )
}

test normal_partial_market_swap_ada_to_token() {
  let tradable_input0 = 1_000_000
  let fee0 = 5000
  let price = rational(0, 1)
  let tradable_input_removed = 200_000
  let input_amount0 = tradable_input0 + fee0
  let fee_removed = fee0
  let input_removed = tradable_input_removed + fee_removed
  let output_amount0 = 10_000
  let input_amount1 = input_amount0 - input_removed
  let output_added = 1
  let output_amount1 = output_amount0 + output_added
  test_partial_swap(
    input: mk_asset(ada_policy_id, ada_asset_name),
    output: mk_asset("x", "x"),
    input_amount: (input_amount0, input_amount1),
    output_amount: (output_amount0, output_amount1),
    lovelace_amount: (input_amount0, input_amount1),
    fee: fee0,
    base_price: price,
  )
}

test normal_partial_swap_token_to_ada() {
  let tradable_input0 = 1_000_000
  let fee0 = 5000
  let steps = 5
  let price = rational(1, 2)
  let tradable_input_removed = 200_000
  let input_amount0 = tradable_input0
  let fee_removed = fee0 * tradable_input_removed / input_amount0
  let input_removed = tradable_input_removed
  let tradable_output0 = 10_000
  let output_amount0 = tradable_output0 + fee0 * steps
  let input_amount1 = input_amount0 - input_removed
  let output_added =
    tradable_input_removed * price.num / price.denom - fee_removed
  let output_amount1 = output_amount0 + output_added
  test_partial_swap(
    input: mk_asset("x", "x"),
    output: mk_asset(ada_policy_id, ada_asset_name),
    input_amount: (input_amount0, input_amount1),
    output_amount: (output_amount0, output_amount1),
    lovelace_amount: (output_amount0, output_amount1),
    fee: fee0,
    base_price: price,
  )
}

test normal_partial_swap_token_to_token() {
  let tradable_input0 = 1_000_000
  let fee0 = 5000
  let steps = 5
  let price = rational(1, 2)
  let tradable_input_removed = 200_000
  let input_amount0 = tradable_input0
  let fee_removed = fee0 * tradable_input_removed / input_amount0
  let input_removed = tradable_input_removed
  let tradable_output0 = 10_000
  let output_amount0 = tradable_output0
  let lovelace0 = fee0 * steps
  let lovelace1 = lovelace0 - fee_removed
  let input_amount1 = input_amount0 - input_removed
  let output_added = tradable_input_removed * price.num / price.denom
  let output_amount1 = output_amount0 + output_added
  test_partial_swap(
    input: mk_asset("x", "x"),
    output: mk_asset("y", "y"),
    input_amount: (input_amount0, input_amount1),
    output_amount: (output_amount0, output_amount1),
    lovelace_amount: (lovelace0, lovelace1),
    fee: fee0,
    base_price: price,
  )
}

fn test_cancell(
  value: (Value, Value),
  redeemer_address: Address,
  cancellation_pkh: ByteArray,
  signatories: List<ByteArray>,
  cancellation_address: Address,
  cancellation_after: PosixTime,
  transaction_validity_range: ValidityRange,
) -> Bool {
  let conf0 =
    InstantOrderConfig {
      tag: this_tag,
      input: mk_asset("x", "x"),
      output: mk_asset("y", "y"),
      base_price: rational(1, 2),
      fee: 1000,
      min_lovelace,
      redeemer_address,
      cancellation_pkh,
      permitted_executors: #"4be4fa25f029d14c0d723af4a1e6fa7133fc3a610f880336ad685cba",
      cancellation_after,
    }
  let address = from_script("spending_script")
  let output0 =
    Output {
      address,
      value: value.1st,
      datum: InlineDatum(conf0),
      reference_script: None,
    }
  let order_input_ref =
    OutputReference { transaction_id: TransactionId(hex32), output_index: 0 }
  let order_input = Input { output_reference: order_input_ref, output: output0 }
  let cancellation_output =
    Output {
      address: cancellation_address,
      value: value.2nd,
      datum: NoDatum,
      reference_script: None,
    }
  let witness = Inline(ScriptCredential("batch_wit"))
  let spending_ctx =
    ScriptContext {
      purpose: Spend(order_input_ref),
      transaction: Transaction {
        ..transaction.placeholder(),
        inputs: [order_input],
        outputs: [cancellation_output],
        extra_signatories: signatories,
        validity_range: transaction_validity_range,
      },
    }
  instant_order(witness, conf0, False, spending_ctx)
}

fn test_partial_swap(
  input: Asset,
  output: Asset,
  input_amount: (Int, Int),
  output_amount: (Int, Int),
  lovelace_amount: (Int, Int),
  fee: Int,
  base_price: Rational,
) -> Bool {
  let conf0 =
    InstantOrderConfig {
      tag: this_tag,
      input,
      output,
      base_price,
      fee,
      min_lovelace,
      redeemer_address: from_verification_key("redeemer"),
      cancellation_pkh: "canceller",
      permitted_executors: pkh1,
      cancellation_after: 0,
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
      address: conf0.redeemer_address,
      value: value1,
      datum: NoDatum,
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
        extra_signatories: [pkh1],
      },
    }
  instant_order(witness, conf0, True, spending_ctx) && batch_witness(
    0,
    withdrawal_ctx,
  )
}

test normal_cancellation() {
  let val = from_lovelace(1000) |> add("tkn", "btc", 1)
  let addr = from_verification_key("dude1")
  let signer = "dude0"
  test_cancell((val, val), addr, signer, [signer], addr, 0, after(0))
}

test bad_cancellation_wrong_redeemer() {
  let val = from_lovelace(1000) |> add("tkn", "btc", 1)
  let addr = from_verification_key("dude1")
  let signer = "dude0"
  !test_cancell(
    (val, val),
    addr,
    signer,
    [signer],
    from_verification_key("dude2"),
    0,
    after(0),
  )
}

test bad_cancellation_premature() {
  let val = from_lovelace(1000) |> add("tkn", "btc", 1)
  let addr = from_verification_key("dude1")
  let signer = "dude0"
  !test_cancell(
    (val, val),
    addr,
    signer,
    [signer],
    from_verification_key("dude2"),
    1,
    after(0),
  )
}

test bad_cancellation_unauthorized() {
  let val = from_lovelace(1000) |> add("tkn", "btc", 1)
  let addr = from_verification_key("dude1")
  let signer = "dude0"
  !test_cancell((val, val), addr, signer, ["dude2"], addr, 0, after(0))
}

fn test_mixed_full_swap(
  tradable_input: Int,
  fee: Int,
  base_price: Rational,
  removed_tradable_input: Int,
  removed_ex_fee: Int,
  added_output: Int,
  permitted_executors: VerificationKeyHash,
  executor: VerificationKeyHash,
) -> Bool {
  let (initial_order_utxo, initial_conf) =
    make_order(
      tradable_input,
      fee,
      base_price,
      test_redeemer,
      permitted_executors,
    )
  let order_input_ref =
    OutputReference { transaction_id: TransactionId(hex32), output_index: 0 }
  let order_input =
    Input { output_reference: order_input_ref, output: initial_order_utxo }
  let removed_input = removed_tradable_input + removed_ex_fee
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
  let wdrl_ctx =
    ScriptContext { purpose: WithdrawFrom(wit), transaction: ctx.transaction }
  instant_order(wit, initial_conf, True, ctx) && batch_witness(0, wdrl_ctx)
}

fn test_batched_full_swap(
  tradable_input: Int,
  fee: Int,
  base_price: Rational,
  removed_tradable_input: Int,
  removed_ex_fee: Int,
  added_output: Int,
  permitted_executors: VerificationKeyHash,
  executor: VerificationKeyHash,
  size: Int,
) -> Bool {
  let (initial_order_utxo, initial_conf) =
    make_order(
      tradable_input,
      fee,
      base_price,
      test_redeemer,
      permitted_executors,
    )
  let order_input_ref =
    OutputReference { transaction_id: TransactionId(hex32), output_index: 0 }
  let order_input =
    Input { output_reference: order_input_ref, output: initial_order_utxo }
  let removed_input = removed_tradable_input + removed_ex_fee
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
  let wdrl_ctx =
    ScriptContext { purpose: WithdrawFrom(wit), transaction: ctx.transaction }
  list.foldl(
    list.range(0, size),
    True,
    fn(_x, acc) { acc && instant_order(wit, initial_conf, True, ctx) },
  ) && batch_witness(0, wdrl_ctx)
}

test valid_batched_full_swap_at_base_price() {
  let tradable_input = 1_000_000_000
  let base_price = rational(100, 100)
  let fee = 1_000_000

  let removed_tradable_input = tradable_input
  let added_output = removed_tradable_input * base_price.num / base_price.denom
  let removed_ex_fee = fee
  let allowed_executors = pkh1
  test_batched_full_swap(
    tradable_input,
    fee,
    base_price,
    removed_tradable_input,
    removed_ex_fee,
    added_output,
    allowed_executors,
    pkh1,
    60,
  )
}

test valid_full_swap_at_base_price() {
  let tradable_input = 1_000_000_000
  let base_price = rational(61, 100)
  let fee = 1_000_000

  let removed_tradable_input = tradable_input
  let added_output = removed_tradable_input * base_price.num / base_price.denom
  let removed_ex_fee = fee
  let allowed_executors = pkh1
  test_batched_full_swap(
    tradable_input,
    fee,
    base_price,
    removed_tradable_input,
    removed_ex_fee,
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

  let removed_tradable_input = tradable_input
  let added_output = removed_tradable_input * base_price.num / base_price.denom
  let removed_ex_fee = fee
  let allowed_executors = pkh1
  test_mixed_full_swap(
    tradable_input,
    fee,
    base_price,
    removed_tradable_input,
    removed_ex_fee,
    added_output,
    allowed_executors,
    pkh1,
  )
}

test invalid_full_swap_bad_exchange_rate() {
  let tradable_input = 1_000_000_000
  let base_price = rational(100, 100)
  let fee = 1_000_000

  let removed_tradable_input = tradable_input
  let added_output =
    removed_tradable_input * base_price.num / base_price.denom - 1
  let removed_ex_fee = fee
  let allowed_executors = pkh1
  !test_batched_full_swap(
    tradable_input,
    fee,
    base_price,
    removed_tradable_input,
    removed_ex_fee,
    added_output,
    allowed_executors,
    pkh1,
    50,
  )
}
