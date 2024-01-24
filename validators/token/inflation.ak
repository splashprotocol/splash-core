use aiken/interval
use aiken/list
use aiken/math
use aiken/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, ScriptContext, Spend,
  Transaction, TransactionId, ValidityRange, find_input,
}
use aiken/transaction/credential.{Address, ScriptCredential, from_script}
use aiken/transaction/value.{add, from_lovelace, quantity_of}

///    [Emission Script]
///            |
///  [Emission Bin {Epoch}]
type Release {
  /// Index of the successive inflation output.
  successor_ix: Int,
  /// Index of the emission bin where emitted tokens are transfered.
  emission_bin_ix: Int,
}

/// Length of one emission epoch in milliseconds.
const epoch_len = 86_400_000

/// Length of the emission reduction period in epochs.
const emission_reduction_period_len = 91

const total_emission = 32_000_000_000_000

/// Dayly inflation rate in micro-SPLASH.
const rate_initial = 32_000_000_000

const rate_after_first_reduction = 22_400_000_000

/// Reduction rate applied after first reduction.
const tail_reduction_rate_num = 94524

const tail_reduction_rate_den = 100_000

validator(
  /// Minting policy.
  policy: ByteArray,
  /// Token name.
  name: ByteArray,
  /// Script hash of emission bin.
  emission_bin_sh: ByteArray,
  /// Posix timestamp when first emission occurs.
  zeroth_epoch_start: Int,
) {
  fn inflation(epoch: Int, action: Release, ctx: ScriptContext) -> Bool {
    let ScriptContext { transaction, purpose } = ctx
    expect Spend(spent_utxo_reference) = purpose
    expect Some(self_input) =
      find_input(transaction.inputs, spent_utxo_reference)
    let self = self_input.output
    expect Some(successor) = list.at(transaction.outputs, action.successor_ix)
    // Validate emission epoch:
    let current_epoch_start = zeroth_epoch_start + epoch * epoch_len
    let valid_epoch =
      interval.is_entirely_after(
        transaction.validity_range,
        current_epoch_start,
      )
    // Validate emission:
    let reserves_self = quantity_of(self.value, policy, name)
    let reserves_succ = quantity_of(successor.value, policy, name)
    let amount_released = reserves_self - reserves_succ
    let valid_emission =
      amount_released == math.min(emission_rate(epoch), reserves_self)
    // Validate successor:
    let valid_successor_script = successor.address == self.address
    let valid_successor_epoch = successor.datum == InlineDatum(epoch + 1)
    let emission_exhausted = reserves_succ == 0
    let valid_successor =
      emission_exhausted || valid_successor_script && valid_successor_epoch
    // Validate emission bin:
    expect Some(emission_bin) =
      list.at(transaction.outputs, action.emission_bin_ix)
    expect ScriptCredential(actual_emission_bin_script) =
      emission_bin.address.payment_credential
    let valid_emission_bin_script =
      actual_emission_bin_script == emission_bin_sh
    expect InlineDatum(emission_bin_epoch) = emission_bin.datum
    expect emission_bin_epoch: Int = emission_bin_epoch
    let valid_emission_bin_epoch = emission_bin_epoch == epoch
    let valid_emission_bin_balance =
      quantity_of(emission_bin.value, policy, name) == amount_released
    valid_epoch? && valid_emission? && valid_successor? && valid_emission_bin_script? && valid_emission_bin_epoch? && valid_emission_bin_balance?
  }
}

/// Calculate emission rate based on given epoch.
fn emission_rate(epoch: Int) -> Int {
  let reduction_period = epoch / emission_reduction_period_len
  if reduction_period == 0 {
    rate_initial
  } else if reduction_period == 1 {
    rate_after_first_reduction
  } else {
    let exp = list.range(0, reduction_period - 2)
    // We calculate numerator/denominator separately to avoid error accumulation.
    let num =
      list.reduce(
        exp,
        rate_after_first_reduction,
        fn(acc, _x) { acc * tail_reduction_rate_num },
      )
    let denom =
      list.reduce(exp, 1, fn(acc, _x) { acc * tail_reduction_rate_den })
    num / denom
  }
}

const max_error = 1_000_000_000

const policy =
  #"0000000000000000000000000000000000000000000000000000000000000002"

const name = "splash"

const hash32_1 = #"f4c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50"

const hash32_2 = #"1544d2ed5fc4fafd503ef7334ccab11eb8a0815817aa0822938c4302"

const min_value = 1_000

const emission_start = 1705930274227

const num_reductions = 28

fn test_emission_chain(
  epoch: Int,
  next_epoch: Int,
  emission_box: Output,
  withdrawal: Int,
  withdraw_to_epoch: Int,
  withdraw_to: Address,
  validity_range: ValidityRange,
) -> (Bool, Output) {
  let emission_input_ref =
    OutputReference { transaction_id: TransactionId(hash32_1), output_index: 0 }
  let emission_input =
    Input { output_reference: emission_input_ref, output: emission_box }
  let emission_output =
    Output {
      ..emission_box,
      datum: InlineDatum(next_epoch),
      value: add(emission_box.value, policy, name, -withdrawal),
    }
  let bin_output =
    Output {
      address: withdraw_to,
      value: add(from_lovelace(min_value), policy, name, withdrawal),
      datum: InlineDatum(withdraw_to_epoch),
      reference_script: None,
    }
  let action = Release { successor_ix: 0, emission_bin_ix: 1 }
  let ctx =
    ScriptContext {
      purpose: Spend(emission_input_ref),
      transaction: transaction.placeholder()
        |> fn(transaction) {
             Transaction {
               ..transaction,
               inputs: [emission_input],
               outputs: [emission_output, bin_output],
               validity_range: validity_range,
             }
           },
    }
  (
    inflation(policy, name, hash32_2, emission_start, epoch, action, ctx),
    emission_output,
  )
}

fn test_emission_remainder(
  epoch: Int,
  emission_box: Output,
  withdrawal: Int,
  withdraw_to_epoch: Int,
  withdraw_to: Address,
  validity_range: ValidityRange,
) -> Bool {
  let emission_input_ref =
    OutputReference { transaction_id: TransactionId(hash32_1), output_index: 0 }
  let emission_input =
    Input { output_reference: emission_input_ref, output: emission_box }
  let bin_output =
    Output {
      address: withdraw_to,
      value: add(from_lovelace(min_value), policy, name, withdrawal),
      datum: InlineDatum(withdraw_to_epoch),
      reference_script: None,
    }
  let bait_output =
    Output {
      address: from_script(hash32_1),
      value: from_lovelace(min_value),
      datum: NoDatum,
      reference_script: None,
    }
  let action = Release { successor_ix: 1, emission_bin_ix: 0 }
  let ctx =
    ScriptContext {
      purpose: Spend(emission_input_ref),
      transaction: transaction.placeholder()
        |> fn(transaction) {
             Transaction {
               ..transaction,
               inputs: [emission_input],
               outputs: [bin_output, bait_output],
               validity_range: validity_range,
             }
           },
    }
  inflation(policy, name, hash32_2, emission_start, epoch, action, ctx)
}

test emission_integration() {
  let reductions = list.range(0, 27)
  let emission =
    list.reduce(
      reductions,
      0,
      fn(acc, r) {
        let rate = emission_rate(emission_reduction_period_len * r)
        acc + rate * emission_reduction_period_len
      },
    )
  emission <= total_emission && total_emission - max_error < emission
}

test full_emission() {
  let last_epoch = num_reductions * emission_reduction_period_len - 1
  let epochs = list.range(0, last_epoch)
  let value = add(from_lovelace(min_value), policy, name, total_emission)
  let initial_emission_box =
    Output {
      address: from_script(hash32_1),
      value,
      datum: InlineDatum(0),
      reference_script: None,
    }
  let (passed, accumulated_emission, remainder_box) =
    list.reduce(
      epochs,
      (True, 0, initial_emission_box),
      fn(acc, e) {
        let (acc_res, emission, emission_box) = acc
        let current_emission = emission_rate(e)
        let valid_time = interval.after(emission_start + e * epoch_len + 1)
        let (res, next_emission_box) =
          test_emission_chain(
            e,
            e + 1,
            emission_box,
            current_emission,
            e,
            from_script(hash32_2),
            valid_time,
          )
        (acc_res && res, emission + current_emission, next_emission_box)
      },
    )
  let valid_total_emission =
    accumulated_emission >= total_emission - max_error && accumulated_emission <= total_emission
  let very_last_epoch = last_epoch + 1
  let last_bit_of_emission = quantity_of(remainder_box.value, policy, name)
  let valid_remainder_emission =
    test_emission_remainder(
      very_last_epoch,
      remainder_box,
      last_bit_of_emission,
      very_last_epoch,
      from_script(hash32_2),
      interval.after(emission_start + epoch_len * very_last_epoch + 1),
    )
  passed && valid_total_emission && valid_remainder_emission
}

test valid_tail_emission_step() {
  let e = 26 * emission_reduction_period_len
  let value = add(from_lovelace(min_value), policy, name, total_emission)
  let initial_emission_box =
    Output {
      address: from_script(hash32_1),
      value,
      datum: InlineDatum(e),
      reference_script: None,
    }
  let current_emission = emission_rate(e)
  let (passed, _) =
    test_emission_chain(
      e,
      e + 1,
      initial_emission_box,
      current_emission,
      e,
      from_script(hash32_2),
      interval.after(emission_start + epoch_len * e + 1),
    )
  passed
}

test invalid_epoch_transition() {
  let e = 0
  let value = add(from_lovelace(min_value), policy, name, total_emission)
  let initial_emission_box =
    Output {
      address: from_script(hash32_1),
      value,
      datum: InlineDatum(e),
      reference_script: None,
    }
  let current_emission = emission_rate(e)
  let (passed, _) =
    test_emission_chain(
      e,
      e + 2,
      initial_emission_box,
      current_emission,
      e,
      from_script(hash32_2),
      interval.after(emission_start + 1),
    )
  !passed
}

test invalid_emission() {
  let e = 0
  let value = add(from_lovelace(min_value), policy, name, total_emission)
  let initial_emission_box =
    Output {
      address: from_script(hash32_1),
      value,
      datum: InlineDatum(e),
      reference_script: None,
    }
  let current_emission = emission_rate(e) + 1
  let (passed, _) =
    test_emission_chain(
      e,
      e + 1,
      initial_emission_box,
      current_emission,
      e,
      from_script(hash32_2),
      interval.after(emission_start + 1),
    )
  !passed
}

test invalid_interval() {
  let e = 0
  let value = add(from_lovelace(min_value), policy, name, total_emission)
  let initial_emission_box =
    Output {
      address: from_script(hash32_1),
      value,
      datum: InlineDatum(e),
      reference_script: None,
    }
  let current_emission = emission_rate(e)
  let (passed, _) =
    test_emission_chain(
      e,
      e + 1,
      initial_emission_box,
      current_emission,
      e,
      from_script(hash32_2),
      interval.after(emission_start - 1),
    )
  !passed
}

test invalid_withdrawal_script() {
  let e = 0
  let value = add(from_lovelace(min_value), policy, name, total_emission)
  let initial_emission_box =
    Output {
      address: from_script(hash32_1),
      value,
      datum: InlineDatum(e),
      reference_script: None,
    }
  let current_emission = emission_rate(e)
  let (passed, _) =
    test_emission_chain(
      e,
      e + 1,
      initial_emission_box,
      current_emission,
      e,
      from_script(hash32_1),
      interval.after(emission_start + 1),
    )
  !passed
}

test invalid_withdrawal_epoch() {
  let e = 0
  let value = add(from_lovelace(min_value), policy, name, total_emission)
  let initial_emission_box =
    Output {
      address: from_script(hash32_1),
      value,
      datum: InlineDatum(e),
      reference_script: None,
    }
  let current_emission = emission_rate(e)
  let (passed, _) =
    test_emission_chain(
      e,
      e + 1,
      initial_emission_box,
      current_emission,
      e + 1,
      from_script(hash32_2),
      interval.after(emission_start + 1),
    )
  !passed
}

test invalid_withdrawal_balance() {
  let e = 128
  let value = add(from_lovelace(min_value), policy, name, total_emission)
  let initial_emission_box =
    Output {
      address: from_script(hash32_1),
      value,
      datum: InlineDatum(e),
      reference_script: None,
    }
  let current_emission = emission_rate(e)
  let emission_input_ref =
    OutputReference { transaction_id: TransactionId(hash32_1), output_index: 0 }
  let emission_input =
    Input { output_reference: emission_input_ref, output: initial_emission_box }
  let emission_output =
    Output {
      ..initial_emission_box,
      datum: InlineDatum(e + 1),
      value: add(initial_emission_box.value, policy, name, -current_emission),
    }
  let bin_output =
    Output {
      address: from_script(hash32_2),
      value: add(from_lovelace(min_value), policy, name, current_emission - 1),
      datum: InlineDatum(e),
      reference_script: None,
    }
  let action = Release { successor_ix: 0, emission_bin_ix: 1 }
  let ctx =
    ScriptContext {
      purpose: Spend(emission_input_ref),
      transaction: transaction.placeholder()
        |> fn(transaction) {
             Transaction {
               ..transaction,
               inputs: [emission_input],
               outputs: [emission_output, bin_output],
               validity_range: interval.after(
                 emission_start + e * epoch_len + 1,
               ),
             }
           },
    }
  !inflation(policy, name, hash32_2, emission_start, e, action, ctx)
}
