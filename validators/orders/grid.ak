use aiken/builtin
use aiken/dict
use aiken/list
use aiken/transaction.{
  InlineDatum, Input, Output, OutputReference, ScriptContext, Spend, Transaction,
  TransactionId, find_input,
}
use aiken/transaction/credential.{Address, from_script, from_verification_key}
use aiken/transaction/value.{PolicyId, Value, add, from_lovelace}
use splash/plutus.{Asset, VerificationKeyHash, mk_asset}
use splash/rational.{Rational, rational}
use splash/value_ext

type GridStateNative {
  beacon: PolicyId,
  token: Asset,
  buy_shift_factor: Rational,
  sell_shift_factor: Rational,
  max_lovelace_offer: Int,
  lovelace_offer: Int,
  price: Rational,
  side: Bool,
  /// Lovelace allowed to be utilized to cover TX fee.
  budget_per_transaction: Int,
  /// Where the output from the order must go.
  redeemer_address: Address,
  /// PKH authorized to cancel order.
  cancellation_pkh: VerificationKeyHash,
}

type Action {
  Execute { successor_out_index: Int }
  Close
}

const max_assets = 3

validator {
  fn grid_native(
    state: GridStateNative,
    action: Action,
    ctx: ScriptContext,
  ) -> Bool {
    let ScriptContext { transaction, purpose } = ctx
    let tx_inputs = transaction.inputs
    let tx_outputs = transaction.outputs
    expect Spend(spent_utxo_reference) = purpose
    when action is {
      Execute { successor_out_index } -> {
        expect Some(self_input) = find_input(tx_inputs, spent_utxo_reference)
        let self = self_input.output
        let self_value = self.value
        expect Some(successor) = list.at(tx_outputs, successor_out_index)
        let successor_value = successor.value

        expect [lovelace_pol_0, ..self_value_raw] =
          self_value |> value.to_dict |> dict.to_list
        let (_, lovelace_0) =
          lovelace_pol_0.2nd |> dict.to_list |> builtin.head_list
        expect [lovelace_pol_1, ..successor_value_raw] =
          successor_value |> value.to_dict |> dict.to_list
        let (_, lovelace_1) =
          lovelace_pol_1.2nd |> dict.to_list |> builtin.head_list

        let GridStateNative {
          beacon,
          token,
          buy_shift_factor,
          sell_shift_factor,
          max_lovelace_offer,
          lovelace_offer,
          price,
          side,
          budget_per_transaction,
          redeemer_address,
          cancellation_pkh,
        } = state

        let Asset { policy, name } = state.token

        let token_0 =
          value_ext.quantity_of(self_value_raw, policy, name, max_assets)
        let token_1 =
          value_ext.quantity_of(successor_value_raw, policy, name, max_assets)

        let nft_preserved =
          value_ext.quantity_of(successor_value_raw, beacon, "", max_assets) == 1

        let delta_lovelace = lovelace_1 - lovelace_0
        let delta_token = token_1 - token_0

        let delta_tradable_lovelace = delta_lovelace + budget_per_transaction

        expect InlineDatum(successor_state) = successor.datum
        let valid_trade =
          when side is {
            True -> {
              let is_sell = delta_token < 0
              let valid_rate =
                delta_tradable_lovelace * price.denom >= -delta_token * price.num
              let quote_sold_completely = token_1 == 0
              let expected_successor_state: Data =
                GridStateNative {
                  beacon: state.beacon,
                  token,
                  price: if quote_sold_completely {
                    rational(
                      price.num * buy_shift_factor.num,
                      price.denom * buy_shift_factor.denom,
                    )
                  } else {
                    price
                  },
                  buy_shift_factor,
                  sell_shift_factor,
                  lovelace_offer: max_lovelace_offer,
                  max_lovelace_offer,
                  side: !quote_sold_completely,
                  budget_per_transaction,
                  redeemer_address,
                  cancellation_pkh,
                }
              let correct_state_trans =
                successor_state == expected_successor_state
              is_sell? && valid_rate? && correct_state_trans?
            }
            False -> {
              let neg_delta_tradable_lovelace = -delta_tradable_lovelace
              let is_buy = delta_tradable_lovelace < 0
              let valid_rate =
                delta_token * price.num >= neg_delta_tradable_lovelace * price.denom
              let max_lovelace_sold =
                neg_delta_tradable_lovelace >= state.lovelace_offer
              let price_shift_factor = state.sell_shift_factor
              let expected_successor_state: Data =
                GridStateNative {
                  beacon: state.beacon,
                  token,
                  price: if max_lovelace_sold {
                    rational(
                      price.num * price_shift_factor.num,
                      price.denom * price_shift_factor.denom,
                    )
                  } else {
                    price
                  },
                  buy_shift_factor,
                  sell_shift_factor,
                  max_lovelace_offer,
                  lovelace_offer: lovelace_offer + delta_tradable_lovelace,
                  side: max_lovelace_sold,
                  budget_per_transaction,
                  redeemer_address,
                  cancellation_pkh,
                }
              let correct_state_trans =
                successor_state == expected_successor_state
              is_buy? && valid_rate? && correct_state_trans?
            }
          }
        nft_preserved? && valid_trade
      }
      Close -> {
        let is_authorized =
          list.has(transaction.extra_signatories, state.cancellation_pkh)
        expect Spend(spent_utxo_reference) = ctx.purpose
        expect Some(self_input) = find_input(tx_inputs, spent_utxo_reference)
        let output = builtin.head_list(tx_outputs)
        let value_returned = self_input.output.value == output.value
        let correct_redeemer = output.address == state.redeemer_address
        is_authorized && value_returned && correct_redeemer
      }
    }
  }
}

fn base_state() -> GridStateNative {
  GridStateNative {
    beacon: "",
    token: mk_asset("abc", "nike"),
    buy_shift_factor: rational(1000, 1002),
    sell_shift_factor: rational(1, 1),
    max_lovelace_offer: 1000,
    lovelace_offer: 1000,
    price: rational(1, 1),
    side: False,
    budget_per_transaction: 1000,
    redeemer_address: from_verification_key("dude1"),
    cancellation_pkh: "abcd",
  }
}

/// Lovelace allowed to be utilized to cover TX fee.
/// Where the output from the order must go.
/// PKH authorized to cancel order.
fn test_grid_apply(
  value: (Value, Value),
  addr: (Address, Address),
  state: (GridStateNative, GridStateNative),
) -> Bool {
  let order_utxo =
    Output {
      address: addr.1st,
      value: value.1st,
      datum: InlineDatum(state.1st),
      reference_script: None,
    }
  let order_input_ref =
    OutputReference { transaction_id: TransactionId("foo"), output_index: 0 }
  let order_input =
    Input { output_reference: order_input_ref, output: order_utxo }
  let order_output =
    Output {
      address: addr.2nd,
      value: value.2nd,
      datum: InlineDatum(state.2nd),
      reference_script: None,
    }
  let spending_ctx =
    ScriptContext {
      purpose: Spend(order_input_ref),
      transaction: Transaction {
        ..transaction.placeholder(),
        inputs: [order_input],
        outputs: [order_output],
      },
    }
  let redeemer = Execute { successor_out_index: 0 }
  grid_native(state.1st, redeemer, spending_ctx)
}

test legit_trade() {
  let addr = from_script("grid_order_script")
  let beacon = "abc"
  let bs = base_state()
  let tradable_lov = 10_000_000_000
  let p = rational(1, 1)
  let budget = 500_000
  let num_steps_reserved = 10
  let buy_shift_factor = rational(1000, 1002)
  let sell_shift_factor = rational(998, 1000)
  let s0 =
    GridStateNative {
      ..bs,
      beacon: beacon,
      buy_shift_factor: buy_shift_factor,
      sell_shift_factor: sell_shift_factor,
      max_lovelace_offer: tradable_lov,
      lovelace_offer: tradable_lov,
      price: p,
      side: False,
      budget_per_transaction: budget,
    }
  let value0 =
    from_lovelace(tradable_lov + budget * num_steps_reserved)
      |> add(beacon, "", 1)
  let s1 =
    GridStateNative {
      ..bs,
      beacon: beacon,
      buy_shift_factor: buy_shift_factor,
      sell_shift_factor: sell_shift_factor,
      max_lovelace_offer: tradable_lov,
      lovelace_offer: 0,
      price: rational(
        s0.price.num * sell_shift_factor.num,
        s0.price.denom * sell_shift_factor.denom,
      ),
      side: True,
      budget_per_transaction: budget,
    }
  let execution1 = tradable_lov * s0.price.num / s0.price.denom
  let value1 =
    from_lovelace(budget * ( num_steps_reserved - 1 ))
      |> add(beacon, "", 1)
      |> add(bs.token.policy, bs.token.name, execution1)
  let s2 =
    GridStateNative {
      ..bs,
      beacon: beacon,
      buy_shift_factor: buy_shift_factor,
      sell_shift_factor: sell_shift_factor,
      max_lovelace_offer: tradable_lov,
      lovelace_offer: tradable_lov,
      price: rational(
        s1.price.num * buy_shift_factor.num,
        s1.price.denom * buy_shift_factor.denom,
      ),
      side: False,
      budget_per_transaction: budget,
    }
  let execution2 = execution1 * s1.price.denom / s1.price.num
  let value2 =
    from_lovelace(execution2 + budget * ( num_steps_reserved - 2 ))
      |> add(beacon, "", 1)
  test_grid_apply((value0, value1), (addr, addr), (s0, s1)) && test_grid_apply(
    (value1, value2),
    (addr, addr),
    (s1, s2),
  )
}

test legit_partial_trade() {
  let addr = from_script("grid_order_script")
  let beacon = "abc"
  let bs = base_state()
  let tradable_lov = 10_000_000_000
  let p = rational(1, 1)
  let budget = 500_000
  let num_steps_reserved = 10
  let buy_shift_factor = rational(1000, 1002)
  let sell_shift_factor = rational(998, 1000)
  let s0 =
    GridStateNative {
      ..bs,
      beacon: beacon,
      buy_shift_factor: buy_shift_factor,
      sell_shift_factor: sell_shift_factor,
      max_lovelace_offer: tradable_lov,
      lovelace_offer: tradable_lov,
      price: p,
      side: False,
      budget_per_transaction: budget,
    }
  let value0 =
    from_lovelace(tradable_lov + budget * num_steps_reserved)
      |> add(beacon, "", 1)
  let amount_trade1 = tradable_lov / 2
  let s1 =
    GridStateNative {
      ..bs,
      beacon: beacon,
      buy_shift_factor: buy_shift_factor,
      sell_shift_factor: sell_shift_factor,
      max_lovelace_offer: tradable_lov,
      lovelace_offer: tradable_lov - amount_trade1,
      price: s0.price,
      side: False,
      budget_per_transaction: budget,
    }
  let execution1 = amount_trade1 * s0.price.num / s0.price.denom
  let value1 =
    from_lovelace(amount_trade1 + budget * ( num_steps_reserved - 1 ))
      |> add(beacon, "", 1)
      |> add(bs.token.policy, bs.token.name, execution1)
  let s2 =
    GridStateNative {
      ..bs,
      beacon: beacon,
      buy_shift_factor: buy_shift_factor,
      sell_shift_factor: sell_shift_factor,
      max_lovelace_offer: tradable_lov,
      lovelace_offer: 0,
      price: rational(
        s1.price.num * sell_shift_factor.num,
        s1.price.denom * sell_shift_factor.denom,
      ),
      side: True,
      budget_per_transaction: budget,
    }
  let execution2 = execution1 * s1.price.num / s1.price.denom
  let value2 =
    from_lovelace(budget * ( num_steps_reserved - 2 ))
      |> add(beacon, "", 1)
      |> add(bs.token.policy, bs.token.name, execution1 + execution2)
  test_grid_apply((value0, value1), (addr, addr), (s0, s1)) && test_grid_apply(
    (value1, value2),
    (addr, addr),
    (s1, s2),
  )
}

test bad_rate() {
  let addr = from_script("grid_order_script")
  let beacon = "abc"
  let bs = base_state()
  let tradable_lov = 10_000_000_000
  let p = rational(1, 1)
  let budget = 500_000
  let num_steps_reserved = 10
  let buy_shift_factor = rational(1000, 1002)
  let sell_shift_factor = rational(998, 1000)
  let s0 =
    GridStateNative {
      ..bs,
      beacon: beacon,
      buy_shift_factor: buy_shift_factor,
      sell_shift_factor: sell_shift_factor,
      max_lovelace_offer: tradable_lov,
      lovelace_offer: tradable_lov,
      price: p,
      side: False,
      budget_per_transaction: budget,
    }
  let value0 =
    from_lovelace(tradable_lov + budget * num_steps_reserved)
      |> add(beacon, "", 1)
  let s1 =
    GridStateNative {
      ..bs,
      beacon: beacon,
      buy_shift_factor: buy_shift_factor,
      sell_shift_factor: sell_shift_factor,
      max_lovelace_offer: tradable_lov,
      lovelace_offer: 0,
      price: rational(
        s0.price.num * sell_shift_factor.num,
        s0.price.denom * sell_shift_factor.denom,
      ),
      side: True,
      budget_per_transaction: budget,
    }
  let execution1 = tradable_lov * s0.price.num / s0.price.denom
  let value1 =
    from_lovelace(budget * ( num_steps_reserved - 1 ))
      |> add(beacon, "", 1)
      |> add(bs.token.policy, bs.token.name, execution1 - 1)
  !test_grid_apply((value0, value1), (addr, addr), (s0, s1))
}
