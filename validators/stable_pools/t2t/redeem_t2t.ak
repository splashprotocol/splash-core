use aiken/list
use aiken/transaction.{InlineDatum, Output, ScriptContext, Transaction}
use aiken/transaction/credential.{Inline, VerificationKeyCredential}
use aiken/transaction/value.{quantity_of}
use splash/plutus.{Asset}
use splash/stableswap/types.{
  ApplyOrder, CancelOrder, OrderAction, PoolDataT2T, RedeemDataT2T,
}

// LP tokens emisson:
const lp_emission = 9223372036854775807

validator {
  fn stable_redeem_t2t(
    datum: RedeemDataT2T,
    action: OrderAction,
    ctx: ScriptContext,
  ) -> Bool {
    let ScriptContext { transaction, .. } = ctx
    when action is {
      ApplyOrder { redeemer_in_ix, redeemer_out_ix, pool_in_ix } -> {
        let Transaction { inputs, outputs, .. } = transaction
        // Input pool data:
        expect Some(pool_input) = list.at(inputs, pool_in_ix)
        expect Output {
          value: pool_value,
          datum: InlineDatum(pool_datum_in),
          ..
        } = pool_input.output
        expect pool_datum: PoolDataT2T = pool_datum_in
        let PoolDataT2T {
          asset_x,
          asset_y,
          lp_token,
          protocol_fees_x,
          protocol_fees_y,
          ..
        } = pool_datum

        let Asset { policy: asset_x_policy, name: asset_x_name } = asset_x
        let Asset { policy: asset_y_policy, name: asset_y_name } = asset_y
        let Asset { policy: lp_token_policy, name: lp_token_name } = lp_token

        let x_total0 =
          value.quantity_of(pool_value, asset_x_policy, asset_x_name) - protocol_fees_x
        let y_total0 =
          value.quantity_of(pool_value, asset_y_policy, asset_y_name) - protocol_fees_y
        let lp_total0 =
          value.quantity_of(pool_value, lp_token_policy, lp_token_name)

        // Input order data:
        expect Some(self_input) = list.at(inputs, redeemer_in_ix)
        let RedeemDataT2T {
          pool_nft,
          min_expected_x,
          min_expected_y,
          redeemer_pk,
          redeemer_sc,
          ex_fee,
          min_utxo,
        } = datum

        let Asset { policy: pool_nft_policy, name: pool_nft_name } = pool_nft
        let Output { value: self_value, .. } = self_input.output

        let lp_order = quantity_of(self_value, lp_token_policy, lp_token_name)

        // Output user data:
        expect Some(user_output) = list.at(outputs, redeemer_out_ix)
        let Output { value, address: user_output_address, .. } = user_output

        let received_x_total = quantity_of(value, asset_x_policy, asset_x_name)
        let received_y_total = quantity_of(value, asset_y_policy, asset_y_name)

        let (received_x, received_y) =
          if asset_x_policy == #"" && asset_x_name == #"" {
            (received_x_total - ex_fee - min_utxo, received_y_total)
          } else if asset_y_policy == #"" && asset_y_name == #"" {
            (received_x_total, received_y_total - ex_fee - min_utxo)
          } else {
            (received_x_total, received_y_total)
          }

        // Validations:
        // 1. Redeemed from the valid pool:
        let valid_pool: Bool =
          quantity_of(pool_value, pool_nft_policy, pool_nft_name) == 1
        // 2. Valid expected tokens and their amount in user's output:
        let min_expected_amounts_received: Bool =
          received_x >= min_expected_x && received_y >= min_expected_y
        // 3. Valid inputs:
        let valid_inputs: Bool = list.length(inputs) == 2
        // 4. Valid received amount:
        let supply_lp0 = lp_emission - lp_total0
        let valid_rec: Bool =
          lp_order * x_total0 >= supply_lp0 * received_x && lp_order * y_total0 >= supply_lp0 * received_y
        // 5. Valid successor:
        expect VerificationKeyCredential(successor_pay_cred) =
          user_output_address.payment_credential
        expect Some(Inline(VerificationKeyCredential(successor_stake_cred))) =
          user_output_address.stake_credential
        let valid_successor: Bool =
          successor_pay_cred == redeemer_pk && successor_stake_cred == redeemer_sc

        valid_pool && min_expected_amounts_received && valid_inputs && valid_rec && valid_successor
      }
      CancelOrder -> {
        let RedeemDataT2T { redeemer_pk, .. } = datum
        let Transaction { extra_signatories, .. } = transaction
        list.has(extra_signatories, redeemer_pk)
      }
    }
  }
}
