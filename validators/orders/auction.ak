//// Dutch auction order.
//// Price is a function of time. Usually auction starts from a price slightly higher than the spot price
//// and decreses with time by some constant step until it reaches minimal acceptable price. 

use aiken/interval
use aiken/list
use aiken/transaction.{Input, ScriptContext, Spend, find_input}
use aiken/transaction/credential.{ScriptCredential, VerificationKeyCredential}
use aiken/transaction/value.{ada_policy_id, lovelace_of, quantity_of}
use splash/plutus.{Asset, VerificationKeyHash}
use splash/rational.{Rational}

type AuctionRedeemer {
  AuctionRedeemerKey(VerificationKeyHash)
  AuctionRedeemerToken(Asset)
}

type Config {
  // What we swap.
  base: Asset,
  // What we receive.
  quote: Asset,
  // Initial price of the auction (Quote/Base).
  price_start: Rational,
  // Start time of the auction.
  start_time: Int,
  // Length of each step in auction in seconds.
  step_len: Int,
  // Total number of steps in auction.
  steps: Int,
  // Numerator of the negative part of price decay.
  price_dacay_num: Int,
  // Fee per one unit of quote asset.
  fee_per_quote: Rational,
  // Redeemer PK or token.
  redeemer: AuctionRedeemer,
}

type Action {
  /// Execute order
  Exec { span_ix: Int, successor_ix: Int }
  Cancel
}

const price_dacay_denom = 1000

validator {
  fn auction(conf: Config, action: Action, ctx: ScriptContext) -> Bool {
    let ScriptContext { transaction, purpose } = ctx
    when action is {
      Exec { span_ix, successor_ix } -> {
        expect Spend(spent_utxo_reference) = purpose
        expect Some(self_input) =
          find_input(transaction.inputs, spent_utxo_reference)
        let step_len = conf.step_len
        let span_low = conf.start_time + step_len * span_ix
        let span_high = span_low + step_len
        let validity_interval = transaction.validity_range
        let valid_span =
          span_ix < conf.steps && interval.is_entirely_after(
            validity_interval,
            span_low,
          ) && interval.is_entirely_before(validity_interval, span_high)

        let self = self_input.output
        expect Some(successor) = list.at(transaction.outputs, successor_ix)

        // Validate swap
        let base_policy = conf.base.policy
        let quote_policy = conf.quote.policy
        let base_0 = quantity_of(self.value, base_policy, conf.base.name)
        let quote_0 = quantity_of(self.value, quote_policy, conf.quote.name)
        let base_1 = quantity_of(successor.value, base_policy, conf.base.name)
        let quote_1 =
          quantity_of(successor.value, quote_policy, conf.quote.name)

        let quote_is_ada = quote_policy == ada_policy_id
        let base_is_ada = base_policy == ada_policy_id
        let fee_per_quote = conf.fee_per_quote
        let quote_added =
          if quote_is_ada {
            ( quote_1 - quote_0 ) * fee_per_quote.denom / (
              fee_per_quote.denom + fee_per_quote.num
            )
          } else {
            quote_1 - quote_0
          }
        let fee = quote_added * fee_per_quote.num / fee_per_quote.denom
        let base_subtracted =
          if base_is_ada {
            base_0 - base_1 - fee
          } else {
            base_0 - base_1
          }
        let valid_fee =
          if quote_is_ada || base_is_ada {
            True
          } else {
            lovelace_of(self.value) - lovelace_of(successor.value) <= fee
          }
        let valid_exchange = quote_added > 0
        let exp = list.range(0, span_ix)
        let price_num =
          list.reduce(
            exp,
            conf.price_start.num,
            fn(acc, _x) { acc * conf.price_dacay_num },
          )
        let price_denom =
          list.reduce(
            exp,
            conf.price_start.denom,
            fn(acc, _x) { acc * price_dacay_denom },
          )
        let valid_execution =
          quote_added * price_denom == base_subtracted * price_num

        let value_delta =
          value.merge(
            value.without_lovelace(successor.value),
            value.negate(value.without_lovelace(self.value)),
          )
        let no_assets_leaked =
          value_delta
            |> value.add(base_policy, conf.base.name, base_subtracted)
            |> value.add(quote_policy, conf.quote.name, -quote_added)
            |> value.is_zero

        // Validate successor
        let valid_successor =
          when successor.address.payment_credential is {
            VerificationKeyCredential(redeemer_cred) ->
              conf.redeemer == AuctionRedeemerKey(redeemer_cred) && base_1 == 0
            ScriptCredential(_) -> and {
                self.address == successor.address,
                self.datum == successor.datum,
                no_assets_leaked,
              }
          }

        valid_span && valid_successor && valid_exchange && valid_execution && valid_fee
      }
      Cancel -> {
        expect Spend(spent_utxo_reference) = purpose
        expect Some(self_input) =
          find_input(transaction.inputs, spent_utxo_reference)
        when conf.redeemer is {
          AuctionRedeemerKey(redeemer_cred) ->
            list.has(ctx.transaction.extra_signatories, redeemer_cred)
          AuctionRedeemerToken(redeemer_asset) ->
            list.any(
              list.delete(ctx.transaction.inputs, self_input),
              fn(input: Input) {
                value.quantity_of(
                  input.output.value,
                  redeemer_asset.policy,
                  redeemer_asset.name,
                ) > 0
              },
            )
        }
      }
    }
  }
}
