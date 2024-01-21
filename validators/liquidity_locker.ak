use aiken/interval
use aiken/list
use aiken/time.{PosixTime}
use aiken/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, ScriptContext, Spend,
  Transaction, TransactionId, ValidityRange, find_input,
}
use aiken/transaction/credential.{
  Address, VerificationKeyCredential, from_script,
}
use aiken/transaction/value.{add, from_lovelace, quantity_of}
use splash/plutus.{VerificationKeyHash}

type Config {
  /// Redeem of locked assets is possible after this timestamp.
  locked_until: PosixTime,
  /// Who is authorized to redeem assets.
  redeemer: VerificationKeyHash,
}

validator {
  fn liquidity_locker(
    conf: Config,
    successor_ix: Int,
    ctx: ScriptContext,
  ) -> Bool {
    let ScriptContext { transaction, purpose } = ctx
    expect Spend(spent_utxo_reference) = purpose
    expect Some(self_input) =
      find_input(transaction.inputs, spent_utxo_reference)
    let self = self_input.output
    expect Some(successor) = list.at(transaction.outputs, successor_ix)
    let valid_action =
      if self.address == successor.address {
        // Transfer or Relock
        expect InlineDatum(successor_datum) = successor.datum
        expect successor_conf: Config = successor_datum
        let successor_value = successor.value
        let assets_at_least_preserved =
          list.all(
            value.flatten(self.value),
            fn(t3) {
              let (policy, name, self_amount) = t3
              let successor_amount = quantity_of(successor_value, policy, name)
              successor_amount >= self_amount
            },
          )
        let valid_unlock_time = successor_conf.locked_until >= conf.locked_until
        assets_at_least_preserved? && valid_unlock_time?
      } else {
        // Redemption
        interval.is_entirely_after(
          transaction.validity_range,
          conf.locked_until,
        )?
      }
    let is_authorized =
      list.has(ctx.transaction.extra_signatories, conf.redeemer)

    is_authorized? && valid_action
  }
}

const policy =
  #"0000000000000000000000000000000000000000000000000000000000000002"

const name = "usd"

const hex32 = #"f4c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50"

const pkh1 = #"f686d9aa9f784c7e3dc0027c57505444c0383776d159733776226ed2"

const pkh2 = #"1544d2ed5fc4fafd503ef7334ccab11eb8a0815817aa0822938c4302"

const min_value = 1_000

fn make_lock(
  amount: Int,
  locked_until: PosixTime,
  redeemer: VerificationKeyHash,
) -> (Output, Config) {
  let conf = Config { locked_until, redeemer }
  let address = from_script(hex32)
  let value = add(from_lovelace(min_value), policy, name, amount)
  let output =
    Output { address, value, datum: InlineDatum(conf), reference_script: None }
  (output, conf)
}

fn test_redeem(
  amount: Int,
  locked_until: PosixTime,
  redeemer: VerificationKeyHash,
  signatories: List<VerificationKeyHash>,
  validity_range: ValidityRange,
) -> Bool {
  let (initial_lock_utxo, initial_conf) =
    make_lock(amount, locked_until, redeemer)
  let order_input_ref =
    OutputReference { transaction_id: TransactionId(hex32), output_index: 0 }
  let order_input =
    Input { output_reference: order_input_ref, output: initial_lock_utxo }
  let order_output =
    Output {
      address: Address {
        payment_credential: VerificationKeyCredential(pkh2),
        stake_credential: None,
      },
      value: from_lovelace(100),
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
               extra_signatories: signatories,
               validity_range: validity_range,
             }
           },
    }
  liquidity_locker(initial_conf, 0, ctx)
}

fn test_relock_or_transfer(
  amount: Int,
  locked_until: PosixTime,
  redeemer: VerificationKeyHash,
  new_redeemer: VerificationKeyHash,
  asset_delta: Int,
  time_delta: Int,
  signatories: List<VerificationKeyHash>,
) -> Bool {
  let (initial_lock_utxo, initial_conf) =
    make_lock(amount, locked_until, redeemer)
  let order_input_ref =
    OutputReference { transaction_id: TransactionId(hex32), output_index: 0 }
  let order_input =
    Input { output_reference: order_input_ref, output: initial_lock_utxo }
  let updated_conf =
    Config {
      redeemer: new_redeemer,
      locked_until: initial_conf.locked_until + time_delta,
    }
  let order_output =
    Output {
      ..initial_lock_utxo,
      value: add(initial_lock_utxo.value, policy, name, asset_delta),
      datum: InlineDatum(updated_conf),
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
               extra_signatories: signatories,
             }
           },
    }
  liquidity_locker(initial_conf, 0, ctx)
}

test valid_redeem() {
  let locked_until = 100
  test_redeem(
    1000,
    locked_until,
    pkh1,
    [pkh1],
    interval.after(locked_until + 1),
  )
}

test premature_redeem() {
  let locked_until = 100
  !test_redeem(1000, locked_until, pkh1, [pkh1], interval.after(locked_until))
}

test unauthorized_redeem() {
  let locked_until = 100
  !test_redeem(1000, locked_until, pkh1, [], interval.after(locked_until + 1))
}

test valid_relock() {
  let locked_until = 100
  test_relock_or_transfer(1000, locked_until, pkh1, pkh1, 500, 1, [pkh1])
}

test valid_transfer() {
  let locked_until = 100
  test_relock_or_transfer(1000, locked_until, pkh1, pkh2, 0, 0, [pkh1])
}

test valid_transfer_and_relock() {
  let locked_until = 100
  test_relock_or_transfer(1000, locked_until, pkh1, pkh2, 100, 1, [pkh1])
}

test invalid_transfer_unauthorized() {
  let locked_until = 100
  !test_relock_or_transfer(1000, locked_until, pkh1, pkh2, 100, 1, [])
}

test invalid_transfer_asset_balance_shrinked() {
  let locked_until = 100
  !test_relock_or_transfer(1000, locked_until, pkh1, pkh2, -1, 1, [pkh1])
}

test invalid_transfer_unlock_time_shrinked() {
  let locked_until = 100
  !test_relock_or_transfer(1000, locked_until, pkh1, pkh2, 1, -1, [pkh1])
}
