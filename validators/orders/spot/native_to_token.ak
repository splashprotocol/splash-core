use aiken/list
use aiken/transaction.{
  Input, Output, ScriptContext, Spend, Transaction, find_input,
}
use aiken/transaction/credential.{
  Address, ScriptCredential, VerificationKeyCredential,
}
use aiken/transaction/value.{PolicyId, lovelace_of, quantity_of}
use splash/orders/spot.{Action, Cancel, Exec, beacon_name}
use splash/plutus.{Asset, VerificationKeyHash}
use splash/rational.{Rational}

type ConfigNativeToToken {
  /// Beacon used to track progress.
  beacon: PolicyId,
  /// Tradable amount of ADA.
  tradable_input: Int,
  /// What we receive.
  output: Asset,
  /// Worst acceptable price (Output/Input).
  base_price: Rational,
  /// How much we pay for each unit of ouput asset.
  fee_per_output: Rational,
  /// Amount of output sufficient for the order to be terminated.
  termination_threshold: Int,
  /// Redeemer PKH.
  redeemer_reward_pkh: VerificationKeyHash,
  /// Redeemer stake PKH.
  redeemer_stake_pkh: List<VerificationKeyHash>,
}

validator {
  fn spot_native_to_token(
    conf: ConfigNativeToToken,
    action: Action,
    ctx: ScriptContext,
  ) -> Bool {
    when action is {
      Exec { successor_ix } -> {
        let ScriptContext { transaction, purpose } = ctx
        expect Spend(spent_utxo_reference) = purpose
        expect Some(self_input) =
          find_input(transaction.inputs, spent_utxo_reference)

        let self = self_input.output
        expect Some(successor) = list.at(transaction.outputs, successor_ix)

        // Validate swap
        let output_policy = conf.output.policy
        let input_0 = lovelace_of(self.value)
        let output_0 = quantity_of(self.value, output_policy, conf.output.name)
        let input_1 = lovelace_of(successor.value)
        let output_1 =
          quantity_of(successor.value, output_policy, conf.output.name)

        let output_added = output_1 - output_0
        let fee_per_output = conf.fee_per_output
        let fee = output_added * fee_per_output.num / fee_per_output.denom
        let input_subtracted = input_0 - input_1 - fee
        // Correct exchange rate.
        let valid_exchange_rate =
          output_added * conf.base_price.denom >= input_subtracted * conf.base_price.num
        let remaining_tradable_input = conf.tradable_input - input_subtracted
        // Only tradable input is used.
        let valid_input = remaining_tradable_input >= 0
        // No reverse trading.
        let valid_exchange_direction = output_added > 0

        let order_satisfied_or_exhausted =
          output_1 >= conf.termination_threshold || remaining_tradable_input == 0

        let valid_successor =
          when successor.address.payment_credential is {
            VerificationKeyCredential(successor_cred) -> {
              let redeemer_correct = conf.redeemer_reward_pkh == successor_cred
              // Order is satisfied or exhausted and accumulated funds are transferred to the right address.
              order_satisfied_or_exhausted && redeemer_correct
            }
            ScriptCredential(_) -> {
              let beacon_preserved =
                quantity_of(successor.value, conf.beacon, beacon_name) == 1
              let script_preserved = self.address == successor.address
              let successor_datum: Data = successor.datum
              expect successor_conf: ConfigNativeToToken = successor_datum
              let valid_tradable_input =
                successor_conf.tradable_input == remaining_tradable_input
              let config_preserved =
                conf.beacon == successor_conf.beacon && conf.output == successor_conf.output && conf.base_price == successor_conf.base_price && conf.fee_per_output == successor_conf.fee_per_output && conf.termination_threshold == successor_conf.termination_threshold && conf.redeemer_reward_pkh == successor_conf.redeemer_reward_pkh && conf.redeemer_stake_pkh == successor_conf.redeemer_stake_pkh
              // Script and configuration are preserved.
              !order_satisfied_or_exhausted && beacon_preserved && script_preserved && valid_tradable_input && config_preserved
            }
          }

        valid_successor && valid_exchange_direction && valid_exchange_rate && valid_input
      }
      Cancel ->
        list.has(ctx.transaction.extra_signatories, conf.redeemer_reward_pkh)
    }
  }
}
