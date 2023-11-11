//// Dutch auction order.
//// Price is a function of time. Usually auction starts from a price slightly higher than the spot price
//// and decreses with time by some constant step until it reaches minimal acceptable price. 

use aiken/interval
use aiken/list
use aiken/transaction.{ScriptContext, Spend, Transaction, find_input}
use aiken/transaction/credential.{ScriptCredential, VerificationKeyCredential}
use aiken/transaction/value.{ada_policy_id, lovelace_of, quantity_of}
use whalepools/plutus.{Asset, VerificationKeyHash}
use whalepools/rational.{Rational}

type Config {
  base: Asset,
  quote: Asset,
  price_start: Rational,
  start_time: Int,
  step_len: Int,
  steps: Int,
  decay_step: Int,
  fee_per_quote: Rational,
  fee_reserved: Int,
  redeemer: VerificationKeyHash,
}

type Action {
  /// Execute order
  Exec { span_ix: Int, successor_ix: Int }
  Cancel
}

validator {
  fn auction(conf: Config, action: Action, ctx: ScriptContext) -> Bool {
    when action is {
      Exec { span_ix, successor_ix } -> {
        let ScriptContext { transaction, purpose } = ctx
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
        let current_price_num = conf.price_start.num - conf.decay_step * span_ix
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
        let valid_execution_price =
          quote_added * conf.price_start.denom == base_subtracted * current_price_num

        // Validate successor
        let valid_successor =
          when successor.address.payment_credential is {
            VerificationKeyCredential(successor_cred) ->
              conf.redeemer == successor_cred && base_1 == 0
            ScriptCredential(_) -> self.address == successor.address
          }

        valid_span && valid_successor && valid_exchange && valid_execution_price && valid_fee
      }
      Cancel -> must_be_signed_by(ctx.transaction, conf.redeemer)
    }
  }
}

fn must_be_signed_by(transaction: Transaction, vk: VerificationKeyHash) {
  list.has(transaction.extra_signatories, vk)
}
