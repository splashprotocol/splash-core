//// A minting policy that allows to mint an asset once and then burn it at any time.

use aiken/list
use aiken/transaction.{
  Input, Mint, NoDatum, Output, OutputReference, ScriptContext, Transaction,
  TransactionId,
}
use aiken/transaction/credential.{from_script}
use aiken/transaction/value.{from_lovelace, from_minted_value, quantity_of}

const beacon_name = #"01"

validator {
  fn beacon(ref: OutputReference, _redeemer: Data, ctx: ScriptContext) {
    expect Mint(policy) = ctx.purpose
    let amount_minted =
      quantity_of(from_minted_value(ctx.transaction.mint), policy, beacon_name)
    if amount_minted > 0 {
      expect Some(first_input) = list.at(ctx.transaction.inputs, 0)
      first_input.output_reference == ref
    } else {
      True
    }
  }
}

const string_28 = #"f4c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50"

test valid_mint() {
  let order_input_ref =
    OutputReference {
      transaction_id: TransactionId(string_28),
      output_index: 0,
    }
  let output_in_inputs =
    Output {
      address: from_script(string_28),
      value: from_lovelace(1000),
      datum: NoDatum,
      reference_script: None,
    }
  let input =
    Input { output_reference: order_input_ref, output: output_in_inputs }
  let ctx =
    ScriptContext {
      purpose: Mint(string_28),
      transaction: transaction.placeholder()
        |> fn(transaction) {
             Transaction {
               ..transaction,
               inputs: [input],
               mint: value.add(value.zero(), string_28, beacon_name, 1)
                 |> value.to_minted_value(),
             }
           },
    }
  beacon(order_input_ref, True, ctx)
}

test valid_burn() {
  let order_input_ref =
    OutputReference {
      transaction_id: TransactionId(string_28),
      output_index: 0,
    }
  let ctx =
    ScriptContext {
      purpose: Mint(string_28),
      transaction: transaction.placeholder()
        |> fn(transaction) {
             Transaction {
               ..transaction,
               mint: value.add(value.zero(), string_28, beacon_name, -1)
                 |> value.to_minted_value(),
             }
           },
    }
  beacon(order_input_ref, True, ctx)
}

test invalid_mint_required_input_not_present() fail {
  let order_input_ref =
    OutputReference {
      transaction_id: TransactionId(string_28),
      output_index: 0,
    }
  let ctx =
    ScriptContext {
      purpose: Mint(string_28),
      transaction: transaction.placeholder()
        |> fn(transaction) {
             Transaction {
               ..transaction,
               mint: value.add(value.zero(), string_28, beacon_name, 1)
                 |> value.to_minted_value(),
             }
           },
    }
  beacon(order_input_ref, True, ctx)
}
