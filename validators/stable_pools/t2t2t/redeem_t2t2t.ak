use aiken/list
use aiken/transaction.{InlineDatum, Output, ScriptContext, Transaction}
use aiken/transaction/credential.{ScriptCredential, VerificationKeyCredential}
use aiken/transaction/value.{quantity_of}
use splash/plutus.{Asset}
use splash/stableswap/types.{
  ApplyOrder, CancelOrder, OrderAction, PoolDataT2T2T, RedeemDataT2T2T,
}

validator {
  fn stable_redeem_t2t2t(
    datum: RedeemDataT2T2T,
    action: OrderAction,
    ctx: ScriptContext,
  ) -> Bool {
    let ScriptContext { transaction, .. } = ctx
    let Transaction { extra_signatories, .. } = transaction
    let RedeemDataT2T2T { redeemer, .. } = datum
    when action is {
      ApplyOrder { redeemer_in_ix, redeemer_out_ix, pool_in_ix } -> {
        let Transaction { inputs, outputs, .. } = transaction

        // Input order data:
        expect Some(self_input) = list.at(inputs, redeemer_in_ix)
        let RedeemDataT2T2T {
          pool_nft,
          min_expected_x,
          min_expected_y,
          min_expected_z,
          ..
        } = datum
        let Asset { policy: pool_nft_policy, name: pool_nft_name } = pool_nft

        // Input pool data:
        expect Some(pool_input) = list.at(inputs, pool_in_ix)
        expect Output {
          value: pool_value,
          datum: InlineDatum(pool_datum_in),
          ..
        } = pool_input.output
        expect pool_datum: PoolDataT2T2T = pool_datum_in
        let PoolDataT2T2T { asset_x, asset_y, asset_z, .. } = pool_datum

        let Asset { policy: asset_x_policy, name: asset_x_name } = asset_x
        let Asset { policy: asset_y_policy, name: asset_y_name } = asset_y
        let Asset { policy: asset_z_policy, name: asset_z_name } = asset_z
        // Output user data:
        expect Some(user_output) = list.at(outputs, redeemer_out_ix)
        let Output { value, address, .. } = user_output

        let received_x = quantity_of(value, asset_x_policy, asset_x_name)
        let received_y = quantity_of(value, asset_y_policy, asset_y_name)
        let received_z = quantity_of(value, asset_z_policy, asset_z_name)

        // Validations:
        // 1. Redeemed from the valid pool:
        let valid_pool: Bool =
          quantity_of(pool_value, pool_nft_policy, pool_nft_name) == 1
        // 2. Valid expected tokens and their amount in user's output:
        let min_expected_amounts_received: Bool =
          received_x >= min_expected_x && received_y >= min_expected_y && received_z >= min_expected_z
        // 3. Valid successor:
        let valid_successor: Bool =
          when user_output.address.payment_credential is {
            VerificationKeyCredential(successor_cred) ->
              datum.redeemer == successor_cred
            ScriptCredential(_) -> self_input.output.address == address
          }

        valid_pool && min_expected_amounts_received && valid_successor
      }
      CancelOrder -> list.has(extra_signatories, redeemer)
    }
  }
}
