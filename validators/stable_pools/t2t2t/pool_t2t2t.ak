use aiken/bytearray
use aiken/int
use aiken/list
use aiken/math
use aiken/transaction.{
  InlineDatum, Input, Output, OutputReference, ScriptContext, Spend, Transaction,
  TransactionId,
}
use aiken/transaction/credential.{Address, from_script}
use aiken/transaction/value.{Value}
use splash/gen.{generate_assets}
use splash/plutus.{Asset}
use splash/stableswap/types.{AMMAction, PDAOAction, PoolDataT2T2T, PoolRedeemer}
use splash/witness.{contains_withdrawal}

// LP tokens emisson:
const lp_emission = 9223372036854775807

// Common denominator for fees calculations:
const denom = 100_000

const n = 3

const nn = 27

/// Main pool validator for pool with 3 tradable assets (X/Y/Z):
/// the validator ensures that the reserves are safu.
validator {
  fn validate_stable_pool_transition_t2t2t(
    input_datum: PoolDataT2T2T,
    redeemer: PoolRedeemer,
    ctx: ScriptContext,
  ) -> Bool {
    let ScriptContext { transaction, purpose } = ctx
    let Transaction { inputs, outputs, .. } = transaction
    expect Spend(pool_in_utxo_reference) = purpose
    let PoolRedeemer { pool_in_ix, pool_out_ix, action } = redeemer

    // ==================================== POOL INPUT ==================================== //
    expect Some(pool_input) = list.at(inputs, pool_in_ix)
    // Pool input params:
    let Input {
      output: pool_input_output,
      output_reference: pool_output_reference,
    } = pool_input
    let Output {
      value: input_value,
      address: Address {
        payment_credential: payment_cred_in,
        stake_credential: stake_cred_in,
      },
      ..
    } = pool_input_output

    let PoolDataT2T2T {
      pool_nft: pool_nft_in,
      asset_x: asset_x_in,
      asset_y: asset_y_in,
      asset_z: asset_z_in,
      multiplier_x: multiplier_x_in,
      multiplier_y: multiplier_y_in,
      multiplier_z: multiplier_z_in,
      lp_token: lp_token_in,
      dao_stabe_proxy_witness: dao_stabe_proxy_witness_in,
      ..
    } = input_datum

    let pool_in_assets =
      input_value |> value.flatten() |> list.map(fn(x) { (x.1st, x.2nd) })

    // ==================================== POOL OUTPUT ==================================== //
    expect Some(pool_out_output) = list.at(outputs, pool_out_ix)
    // Pool output params:
    expect Output {
      value: output_value,
      address: Address {
        payment_credential: payment_cred_out,
        stake_credential: stake_cred_out,
      },
      datum: InlineDatum(pool_out_datum),
      ..
    } = pool_out_output
    expect output_datum: PoolDataT2T2T = pool_out_datum
    let pool_out_assets =
      input_value |> value.flatten() |> list.map(fn(x) { (x.1st, x.2nd) })

    let PoolDataT2T2T {
      pool_nft: pool_nft_out,
      asset_x: asset_x_out,
      asset_y: asset_y_out,
      asset_z: asset_z_out,
      multiplier_x: multiplier_x_out,
      multiplier_y: multiplier_y_out,
      multiplier_z: multiplier_z_out,
      lp_token: lp_token_out,
      dao_stabe_proxy_witness: dao_stabe_proxy_witness_out,
      ..
    } = output_datum

    // ==================================== POOL SELF VALIDATIONS ==================================== //
    // 1. Pool input is valid:
    let valid_pool_input: Bool = pool_in_utxo_reference == pool_output_reference
    // 2. Pool script is preserved:
    let pool_script_preserved: Bool = payment_cred_in == payment_cred_out
    // 3. Pool NFT is preserved:
    let Asset { policy: pool_nft_policy, name: pool_nft_name } = pool_nft_in
    let pool_nft_preserved: Bool =
      value.quantity_of(output_value, pool_nft_policy, pool_nft_name) == 1
    // 4. Pool config params are preserved:
    let pool_config_preserved: Bool =
      pool_nft_in == pool_nft_out && asset_x_in == asset_x_out && asset_y_in == asset_y_out && asset_z_in == asset_z_out && multiplier_x_in == multiplier_x_out && multiplier_y_in == multiplier_y_out && multiplier_z_in == multiplier_z_out && lp_token_in == lp_token_out && dao_stabe_proxy_witness_in == dao_stabe_proxy_witness_out
    // 5. No extra tokens in the pool's value:
    let no_more_tokens_in_pool_out: Bool = pool_in_assets == pool_out_assets
    // 6. Action is valid:
    let valid_action: Bool =
      when action is {
        AMMAction(inv) ->
          validate_amm_operation(
            input_value,
            input_datum,
            output_value,
            output_datum,
            inv,
          ) && stake_cred_in == stake_cred_out

        PDAOAction -> {
          expect Some(dao_stabe_proxy_witness) =
            list.head(dao_stabe_proxy_witness_in)
          contains_withdrawal(transaction.withdrawals, dao_stabe_proxy_witness)
        }
      }
    // Checking whether all validity conditions are met:
    valid_pool_input && pool_script_preserved && pool_nft_preserved && pool_config_preserved && no_more_tokens_in_pool_out && valid_action
  }
}

fn validate_amm_operation(
  input_value: Value,
  input_datum: PoolDataT2T2T,
  output_value: Value,
  output_datum: PoolDataT2T2T,
  invariant: List<Int>,
) -> Bool {
  // ==================================== INPUT DATA ==================================== //
  // Extract values for calculations | Input:
  let PoolDataT2T2T {
    asset_x,
    asset_y,
    asset_z,
    multiplier_x,
    multiplier_y,
    multiplier_z,
    lp_token,
    an2n,
    lp_fee_num,
    protocol_fee_num,
    treasury_address,
    protocol_fees_x: protocol_fees_x0,
    protocol_fees_y: protocol_fees_y0,
    protocol_fees_z: protocol_fees_z0,
    ..
  } = input_datum

  let Asset { policy: lp_token_policy, name: lp_token_name } = lp_token

  // Pool data | Output:
  let PoolDataT2T2T {
    an2n: an2n1,
    lp_fee_num: lp_fee_num1,
    protocol_fee_num: protocol_fee_num1,
    treasury_address: treasury_address1,
    protocol_fees_x: protocol_fees_x1,
    protocol_fees_y: protocol_fees_y1,
    protocol_fees_z: protocol_fees_z1,
    ..
  } = output_datum
  // ==================================== CALCULATING DELTAS ==================================== //
  // Total balances:
  let Asset { policy: asset_x_policy, name: asset_x_name } = asset_x
  let Asset { policy: asset_y_policy, name: asset_y_name } = asset_y
  let Asset { policy: asset_z_policy, name: asset_z_name } = asset_z
  let x_total0 = value.quantity_of(input_value, asset_x_policy, asset_x_name)
  let y_total0 = value.quantity_of(input_value, asset_y_policy, asset_y_name)
  let z_total0 = value.quantity_of(input_value, asset_z_policy, asset_z_name)
  let x_total1 = value.quantity_of(output_value, asset_x_policy, asset_x_name)
  let y_total1 = value.quantity_of(output_value, asset_y_policy, asset_y_name)
  let z_total1 = value.quantity_of(output_value, asset_z_policy, asset_z_name)

  let x_total_delta = x_total1 - x_total0
  let y_total_delta = y_total1 - y_total0
  let z_total_delta = z_total1 - z_total0

  // Tradable balances:
  // We exclude current protocol fees from the total balances for deposit/swap/redeem actions!
  let x_tradable0 = x_total0 - protocol_fees_x0
  let y_tradable0 = y_total0 - protocol_fees_y0
  let z_tradable0 = z_total0 - protocol_fees_z0
  let x_tradable1 = x_total1 - protocol_fees_x1
  let y_tradable1 = y_total1 - protocol_fees_y1
  let z_tradable1 = z_total1 - protocol_fees_z1

  let x_tradable_delta = x_tradable1 - x_tradable0
  let y_tradable_delta = y_tradable1 - y_tradable0
  let z_tradable_delta = z_tradable1 - z_tradable0

  // LP tokens:
  let lp_amount0 =
    value.quantity_of(input_value, lp_token_policy, lp_token_name)
  let lp_amount1 =
    value.quantity_of(output_value, lp_token_policy, lp_token_name)
  let delta_lp = lp_amount1 - lp_amount0
  // Collected protocol fees:
  let protocol_fees_x_delta = protocol_fees_x1 - protocol_fees_x0
  let protocol_fees_y_delta = protocol_fees_y1 - protocol_fees_y0
  let protocol_fees_z_delta = protocol_fees_z1 - protocol_fees_z0

  // ==================================== AMM-ACTION VALIDATIONS ==================================== //
  // 1. Pool mutable params are preserved:
  let pool_mutable_params_preserved: Bool =
    an2n == an2n1 && lp_fee_num == lp_fee_num1 && protocol_fee_num == protocol_fee_num1 && treasury_address == treasury_address1

  // 2. Checking if AMM-action is valid.
  let valid_amm_action =
    if delta_lp != 0 {
      // 2.1. Deposit || Redeem.
      // 2.1.1. Checking the validity of LP token delta:
      let supply_lp0 = lp_emission - lp_amount0
      let supply_lp1 = lp_emission - lp_amount1
      let delta_supply = supply_lp1 - supply_lp0
      let valid_lp_delta: Bool =
        delta_supply * x_tradable0 <= supply_lp0 * x_tradable_delta && delta_supply * y_tradable0 <= supply_lp0 * y_tradable_delta && delta_supply * z_tradable0 <= supply_lp0 * z_tradable_delta

      // 2.1.2. Protocol fees are preserved:
      let protocol_fees_preserved: Bool =
        protocol_fees_x_delta >= 0 && protocol_fees_y_delta >= 0 && protocol_fees_z_delta >= 0

      valid_lp_delta && protocol_fees_preserved
    } else {
      // 2.2. Swap.
      // Base and quote asset info:
      let (
        multiplier_base,
        multiplier_quote,
        multiplier_passive,
        tradable_base_before,
        tradable_quote_before,
        tradable_passive_before,
        tradable_base_after,
        tradable_quote_after,
        tradable_passive_after,
        tradable_base_asset_delta,
        tradable_quote_asset_delta,
        quote_protocol_fees_delta,
        passive_protocol_fees_delta,
        base_total_delta,
        quote_total_delta,
        valid_sgns,
      ) =
        if y_total_delta > 0 && x_total_delta < 0 && z_total_delta == 0 {
          (
            multiplier_y,
            multiplier_x,
            multiplier_z,
            y_tradable0,
            x_tradable0,
            z_tradable0,
            y_tradable1,
            x_tradable1,
            z_tradable1,
            y_tradable_delta,
            x_tradable_delta,
            protocol_fees_x_delta,
            protocol_fees_z_delta,
            y_total_delta,
            x_total_delta,
            True,
          )
        } else if x_total_delta > 0 && y_total_delta < 0 && z_total_delta == 0 {
          (
            multiplier_x,
            multiplier_y,
            multiplier_z,
            x_tradable0,
            y_tradable0,
            z_tradable0,
            x_tradable1,
            y_tradable1,
            z_tradable1,
            x_tradable_delta,
            y_tradable_delta,
            protocol_fees_y_delta,
            protocol_fees_z_delta,
            x_total_delta,
            y_total_delta,
            True,
          )
        } else if z_total_delta > 0 && x_total_delta < 0 && y_total_delta == 0 {
          (
            multiplier_z,
            multiplier_x,
            multiplier_y,
            z_tradable0,
            x_tradable0,
            y_tradable0,
            z_tradable1,
            x_tradable1,
            y_tradable1,
            z_tradable_delta,
            x_tradable_delta,
            protocol_fees_x_delta,
            protocol_fees_y_delta,
            z_total_delta,
            x_total_delta,
            True,
          )
        } else if x_total_delta > 0 && z_total_delta < 0 && y_total_delta == 0 {
          (
            multiplier_x,
            multiplier_z,
            multiplier_y,
            x_tradable0,
            z_tradable0,
            y_tradable0,
            x_tradable1,
            z_tradable1,
            y_tradable1,
            x_tradable_delta,
            z_tradable_delta,
            protocol_fees_z_delta,
            protocol_fees_y_delta,
            x_total_delta,
            z_total_delta,
            True,
          )
        } else if y_total_delta > 0 && z_total_delta < 0 && x_total_delta == 0 {
          (
            multiplier_y,
            multiplier_z,
            multiplier_x,
            y_tradable0,
            z_tradable0,
            x_tradable0,
            y_tradable1,
            z_tradable1,
            x_tradable1,
            y_tradable_delta,
            z_tradable_delta,
            protocol_fees_z_delta,
            protocol_fees_x_delta,
            y_total_delta,
            z_total_delta,
            True,
          )
        } else {
          (
            multiplier_z,
            multiplier_y,
            multiplier_x,
            z_tradable0,
            y_tradable0,
            x_tradable0,
            z_tradable1,
            y_tradable1,
            x_tradable1,
            z_tradable_delta,
            y_tradable_delta,
            protocol_fees_y_delta,
            protocol_fees_x_delta,
            z_total_delta,
            y_total_delta,
            z_total_delta > 0 && y_total_delta < 0 && x_total_delta == 0,
          )
        }

      // 2.2.1. Valid quote protocol fees:
      let total_fees_num_reversed = denom - lp_fee_num - protocol_fee_num
      let valid_quote_protocol_fees: Bool =
        quote_protocol_fees_delta * total_fees_num_reversed >= -quote_total_delta * protocol_fee_num
      // 2.2.2. Non-quote protocol fees are preserved:
      let non_quote_protocol_fees_preserved: Bool =
        tradable_base_asset_delta == base_total_delta && passive_protocol_fees_delta == 0
      // 2.2.4. Validate the StableSwap invariant value (includes validation of the quote liquidity provider fees).
      // Extra '-1' is to ensure that reserves don't decrease as a result of rounding from division
      // when restoring the value of the liquidity provider fees.
      let tradable_quote_after_no_last_lp_fees =
        tradable_quote_after - tradable_quote_asset_delta * lp_fee_num / (
          lp_fee_num - denom
        ) - 1
      expect Some(stableswap_witness) = list.head(invariant)
      let valid_invariant_for_swap: Bool =
        check_invariant_t2t2t(
          n,
          nn,
          an2n,
          stableswap_witness,
          multiplier_base,
          multiplier_quote,
          multiplier_passive,
          tradable_base_before,
          tradable_quote_before,
          tradable_passive_before,
          tradable_base_after,
          tradable_quote_after_no_last_lp_fees,
          tradable_passive_after,
        )
      valid_sgns && valid_quote_protocol_fees && non_quote_protocol_fees_preserved && valid_invariant_for_swap
    }
  pool_mutable_params_preserved && valid_amm_action
}

/// Checks if StableSwap invariant is inside the valid interval:
fn check_invariant_t2t2t(
  n: Int,
  nn: Int,
  an2n: Int,
  d: Int,
  multiplier_x: Int,
  multiplier_y: Int,
  multiplier_z: Int,
  tradable_x_before: Int,
  tradable_y_before: Int,
  tradable_z_before: Int,
  tradable_x_after: Int,
  tradable_y_after: Int,
  tradable_z_after: Int,
) -> Bool {
  let an2n_nn = an2n - nn
  let dn1 = math.pow(d, n + 1)
  let multiplier_xyz = multiplier_x * multiplier_y * multiplier_z

  // Calculate invariant of the initial state: 
  let total_prod_calc_before =
    multiplier_xyz * tradable_x_before * tradable_y_before * tradable_z_before

  let alpha_before = an2n_nn * total_prod_calc_before
  let beta_before =
    an2n * total_prod_calc_before * (
      multiplier_x * tradable_x_before + multiplier_y * tradable_y_before + multiplier_z * tradable_z_before
    )

  // Calculate invariant of the final state: 
  let total_prod_calc_after =
    multiplier_xyz * tradable_x_after * tradable_y_after * tradable_z_after

  let alpha_after = an2n_nn * total_prod_calc_after
  let beta_after =
    an2n * total_prod_calc_after * (
      multiplier_x * tradable_x_after + multiplier_y * tradable_y_after + multiplier_z * tradable_z_after
    )
  // Check that invariant value is between initial and final states:
  dn1 + alpha_before * d - beta_before >= 0 && dn1 + alpha_after * d - beta_after <= 0
}

// ================================================= TESTS ================================================= //

// ============================================ COMMON TEMPLATES ============================================ //

const default_lovelace_value = 1_000_000_000_000_000

/// Returns datum and value with a given params for t2t2t pool:
fn t2t2t_pool_state(
  balances: List<Int>,
  lp_balance: Int,
  pool_nft_policy_string: String,
  pool_nft_name_string: String,
  tradable_assets: List<Asset>,
  tradable_decimals: List<Int>,
  lp_policy_string: String,
  lp_name_string: String,
  an2n: Int,
  lp_fee_num: Int,
  protocol_fee_num: Int,
  dao_policy_string: String,
  treasury_address_string: String,
  protocol_fees: List<Int>,
) -> (PoolDataT2T2T, Value) {
  // Calculate multipliers:
  let tradable_decimals_sorted = list.sort(tradable_decimals, int.compare)
  expect Some(precision) = list.last(tradable_decimals_sorted)
  let tradable_tokens_multipliers =
    list.map(tradable_decimals, fn(x) { precision / x })
  // Make datum:
  expect Some(asset_x) = list.at(tradable_assets, 0)
  expect Some(asset_y) = list.at(tradable_assets, 1)
  expect Some(asset_z) = list.at(tradable_assets, 2)
  expect Some(multiplier_x) = list.at(tradable_tokens_multipliers, 0)
  expect Some(multiplier_y) = list.at(tradable_tokens_multipliers, 1)
  expect Some(multiplier_z) = list.at(tradable_tokens_multipliers, 2)
  expect Some(protocol_fees_x) = list.at(protocol_fees, 0)
  expect Some(protocol_fees_y) = list.at(protocol_fees, 1)
  expect Some(protocol_fees_z) = list.at(protocol_fees, 2)
  let pool_data =
    PoolDataT2T2T {
      pool_nft: Asset {
        policy: bytearray.from_string(pool_nft_policy_string),
        name: bytearray.from_string(pool_nft_name_string),
      },
      asset_x,
      asset_y,
      asset_z,
      multiplier_x,
      multiplier_y,
      multiplier_z,
      lp_token: Asset {
        policy: bytearray.from_string(lp_policy_string),
        name: bytearray.from_string(lp_name_string),
      },
      an2n,
      lp_fee_num,
      protocol_fee_num,
      dao_stabe_proxy_witness: [bytearray.from_string(dao_policy_string)],
      treasury_address: bytearray.from_string(treasury_address_string),
      protocol_fees_x,
      protocol_fees_y,
      protocol_fees_z,
    }
  // Make value:
  expect Some(balance_x) = list.at(balances, 0)
  expect Some(balance_y) = list.at(balances, 1)
  expect Some(balance_z) = list.at(balances, 2)

  let input_value =
    value.from_lovelace(default_lovelace_value)
      |> value.add(pool_data.pool_nft.policy, pool_data.pool_nft.name, 1)
      |> value.add(pool_data.lp_token.policy, pool_data.lp_token.name, lp_balance)
      |> value.add(asset_x.policy, asset_x.name, balance_x)
      |> value.add(asset_y.policy, asset_y.name, balance_y)
      |> value.add(asset_z.policy, asset_z.name, balance_z)

  (pool_data, input_value)
}

/// Returns pool state transition context:
fn make_t2t2t_pool_state_transition_context(
  input_data: PoolDataT2T2T,
  input_value: Value,
  output_data: PoolDataT2T2T,
  output_value: Value,
) -> ScriptContext {
  let pool_addr =
    from_script(#"f4c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50")
  let pool_ref_script =
    Some(#"ee7659342d952e4569acc0592e703737530d1e79fe932673a0fa3e50")
  let placeholder_utxo =
    OutputReference { transaction_id: TransactionId(""), output_index: 0 }
  let pool_input_utxo =
    Output {
      address: pool_addr,
      value: input_value,
      datum: InlineDatum(input_data),
      reference_script: pool_ref_script,
    }

  let pool_output_utxo =
    Output {
      address: pool_addr,
      value: output_value,
      datum: InlineDatum(output_data),
      reference_script: pool_ref_script,
    }

  let pool_input =
    Input { output_reference: placeholder_utxo, output: pool_input_utxo }

  ScriptContext {
    purpose: Spend(placeholder_utxo),
    transaction: transaction.placeholder()
      |> fn(transaction) {
           Transaction {
             ..transaction,
             inputs: [pool_input],
             outputs: [pool_output_utxo],
           }
         },
  }
}

// =================================== SWAP TEST TEMPLATES =================================== //
fn uniform_reserves_valid_swap_math_template(
  n: Int,
  base_amount: Int,
  lp_fee_num_before: Int,
  protocol_fee_num_before: Int,
  base_ind: Int,
  quote_ind: Int,
) -> Bool {
  let min_value = 1_000_000_000
  let default_decimal = 1_000_000_000
  let default_token_value = min_value * default_decimal

  let tradable_assets =
    generate_assets(@"some_pool_token", @"some_pool_token_name", n)

  // Input data:
  let total_balances_before = list.repeat(default_token_value, n)
  let tradable_decimals_before = list.repeat(default_decimal, n)
  let collected_protocol_fees_before = list.repeat(0, n)
  let a_before = 1000
  let an2n = a_before * math.pow(n, 2 * n)

  let inv_before = default_token_value * n
  let lp_balance_before = lp_emission - inv_before

  let (input_pool_data, input_value) =
    t2t2t_pool_state(
      total_balances_before,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
    )
  // Output data:
  let quote_lp_fees = base_amount * input_pool_data.lp_fee_num / denom
  let quote_protocol_fees =
    base_amount * input_pool_data.protocol_fee_num / denom

  let total_balances_after =
    list.indexed_map(
      total_balances_before,
      fn(i, b) {
        if i == base_ind {
          b + base_amount
        } else if i == quote_ind {
          b - base_amount + quote_lp_fees + quote_protocol_fees + 1
        } else {
          b
        }
      },
    )
  let collected_protocol_fees_after =
    list.indexed_map(
      collected_protocol_fees_before,
      fn(i, b) {
        if i == quote_ind {
          b + quote_protocol_fees
        } else {
          b
        }
      },
    )

  let (output_pool_data, output_value) =
    t2t2t_pool_state(
      total_balances_after,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after,
    )

  let ctx =
    make_t2t2t_pool_state_transition_context(
      input_pool_data,
      input_value,
      output_pool_data,
      output_value,
    )
  let redeemer = PoolRedeemer(0, 0, AMMAction([inv_before]))
  // Validation:
  validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data,
    [inv_before],
  ) && validate_stable_pool_transition_t2t2t(input_pool_data, redeemer, ctx)
}

fn uniform_reserves_invalid_swap_math_template(
  n: Int,
  base_amount: Int,
  lp_fee_num_before: Int,
  protocol_fee_num_before: Int,
  base_ind: Int,
  quote_ind: Int,
) -> Bool {
  let default_token_value = 1_000_000_000_000
  let default_decimal = 1_000_000

  let tradable_assets =
    generate_assets(@"some_pool_token", @"some_pool_token_name", n)

  // Input data:
  let total_balances_before = list.repeat(default_token_value, n)
  let tradable_decimals_before = list.repeat(default_decimal, n)
  let collected_protocol_fees_before = list.repeat(0, n)
  let a_before = 1000
  let an2n = a_before * math.pow(n, 2 * n)

  let inv_before = default_token_value * n
  let lp_balance_before = lp_emission - inv_before

  let (input_pool_data, input_value) =
    t2t2t_pool_state(
      total_balances_before,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
    )
  // Output data:
  let quote_lp_fees = base_amount * input_pool_data.lp_fee_num / denom
  let quote_protocol_fees =
    base_amount * input_pool_data.protocol_fee_num / denom

  let collected_protocol_fees_after =
    list.indexed_map(
      collected_protocol_fees_before,
      fn(i, b) {
        if i == quote_ind {
          b + quote_protocol_fees
        } else {
          b
        }
      },
    )

  let total_balances_after =
    list.indexed_map(
      total_balances_before,
      fn(i, b) {
        if i == base_ind {
          b + base_amount
        } else if i == quote_ind {
          b - base_amount + quote_lp_fees + quote_protocol_fees
        } else {
          b
        }
      },
    )
  // Wrong data:
  let total_balances_after_wrong =
    list.indexed_map(
      total_balances_before,
      fn(i, b) {
        if i == base_ind {
          b + base_amount
        } else if i == quote_ind {
          b - base_amount + quote_lp_fees + quote_protocol_fees - 1
        } else {
          b
        }
      },
    )

  let (output_pool_data, output_value) =
    t2t2t_pool_state(
      total_balances_after,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after,
    )
  let (_, output_value_wrong_balance) =
    t2t2t_pool_state(
      total_balances_after_wrong,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after,
    )
  // Wrong protocol fees:
  let collected_protocol_fees_after_wrong =
    list.indexed_map(
      collected_protocol_fees_before,
      fn(i, b) {
        if i == quote_ind {
          b + quote_protocol_fees - n
        } else {
          b
        }
      },
    )
  let (output_pool_data_wrong_protocol_fees, _) =
    t2t2t_pool_state(
      total_balances_after,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after_wrong,
    )
  // Wrong invariants:
  let inv_before_wrong = inv_before + quote_lp_fees - quote_protocol_fees - 1

  // Validations:
  validate_amm_operation(
    input_value,
    input_pool_data,
    output_value_wrong_balance,
    output_pool_data,
    [inv_before],
  ) && validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_protocol_fees,
    [inv_before],
  ) && validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data,
    [inv_before_wrong],
  ) && validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_protocol_fees,
    [inv_before],
  )
}

// // // =================================== DEPOSIT/REDEEM TEST TEMPLATES =================================== //
fn uniform_reserves_valid_lq_math_template(
  n: Int,
  delta: Int,
  a_before: Int,
  lp_fee_num_before: Int,
  protocol_fee_num_before: Int,
  deposit: Bool,
) -> Bool {
  let default_token_value = 1_000_000_000_000_000
  let default_decimal = 1

  let tradable_assets =
    generate_assets(@"some_pool_token", @"some_pool_token_name", n)

  // Input data:
  let total_balances_before = list.repeat(default_token_value, n)
  let tradable_decimals_before = list.repeat(default_decimal, n)
  let collected_protocol_fees_before = list.repeat(0, n)

  let inv_before = default_token_value * n
  let lp_balance_before = lp_emission - inv_before
  let an2n = a_before * math.pow(n, 2 * n)
  let (input_pool_data, input_value) =
    t2t2t_pool_state(
      total_balances_before,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
    )
  // Output data:
  let new_token_value =
    if deposit {
      default_token_value + delta
    } else {
      default_token_value - delta
    }

  let total_balances_after = list.repeat(new_token_value, n)
  let total_balances_after =
    list.indexed_map(
      total_balances_after,
      fn(i, x) {
        if i == 0 {
          x + delta / 2
        } else {
          x
        }
      },
    )

  let lp_balance_after = lp_emission - new_token_value * n
  let lp_balance_after_extra_lp = lp_balance_after + default_token_value
  let (output_pool_data, output_value) =
    t2t2t_pool_state(
      total_balances_after,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
    )

  let (_, output_value_extra_lp) =
    t2t2t_pool_state(
      total_balances_after,
      lp_balance_after_extra_lp,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
    )
  // Validation:
  validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data,
    [],
  ) && validate_amm_operation(
    input_value,
    input_pool_data,
    output_value_extra_lp,
    output_pool_data,
    [],
  )
}

fn uniform_reserves_invalid_lq_math_template(
  n: Int,
  delta: Int,
  a_before: Int,
  lp_fee_num_before: Int,
  protocol_fee_num_before: Int,
  deposit: Bool,
) -> Bool {
  let default_token_value = 1_000_000_000_000_000
  let default_protocol_fee_value = 1_000_000
  let default_decimal = 1

  let tradable_assets =
    generate_assets(@"some_pool_token", @"some_pool_token_name", n)

  // Input data:
  let total_balances_before = list.repeat(default_token_value, n)
  let tradable_decimals_before = list.repeat(default_decimal, n)
  let collected_protocol_fees_before =
    list.repeat(default_protocol_fee_value, n)

  let inv_before = default_token_value * n
  let lp_balance_before = lp_emission - inv_before
  let an2n = a_before * math.pow(n, 2 * n)
  let (input_pool_data, input_value) =
    t2t2t_pool_state(
      total_balances_before,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
    )

  // Output data:
  let new_token_value =
    if deposit {
      default_token_value + delta
    } else {
      default_token_value - delta
    }

  let total_balances_after = list.repeat(new_token_value, n)
  let total_balances_after_wrong =
    list.indexed_map(
      total_balances_after,
      fn(i, x) {
        if i == 0 {
          x - delta / 2
        } else {
          x
        }
      },
    )

  let inv_before = new_token_value * n
  let lp_balance_after = lp_emission - inv_before
  let lp_balance_after_wrong = lp_emission - inv_before - 1

  // Wrong balances:
  let collected_protocol_fees_after_wrong =
    list.repeat(default_token_value - 1, n)

  let (output_pool_data, output_value_wrong_balance) =
    t2t2t_pool_state(
      total_balances_after_wrong,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
    )

  // Wrong protocol fees:
  let (output_pool_data_wrong_protocol_fees, output_value) =
    t2t2t_pool_state(
      total_balances_after,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after_wrong,
    )

  // Wrong LP balance:
  let (_, output_value_wrong_lp) =
    t2t2t_pool_state(
      total_balances_after,
      lp_balance_after_wrong,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
    )
  // Validations:
  validate_amm_operation(
    input_value,
    input_pool_data,
    output_value_wrong_balance,
    output_pool_data,
    [],
  ) && validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_protocol_fees,
    [],
  ) && validate_amm_operation(
    input_value,
    input_pool_data,
    output_value_wrong_lp,
    output_pool_data,
    [],
  )
}

// =================================== DEPOSIT TESTS =================================== //
test test_uniform_deposit_with_valid_math() {
  let a = 3241
  let deposited_amount_list =
    [1_000_000, 950_040_703_214]
  let lp_fee_num = 3123
  let protocol_fee_num = 5127
  let validation_results0 =
    list.map(
      deposited_amount_list,
      fn(delta) {
        uniform_reserves_valid_lq_math_template(
          n,
          delta,
          a,
          lp_fee_num,
          protocol_fee_num,
          True,
        )
      },
    )
  let n_cases = list.length(deposited_amount_list)
  list.count(validation_results0, fn(r) { r == True }) == n_cases
}

test test_uniform_deposit_with_invalid_math() {
  let a = 653

  let lp_fee_num = 123
  let protocol_fee_num = 3141
  let deposited_amount_list =
    [1_000_000, 3_000_000_004, 950_040_703_214]
  let validation_results0 =
    list.map(
      deposited_amount_list,
      fn(delta) {
        uniform_reserves_invalid_lq_math_template(
          n,
          delta,
          a,
          lp_fee_num,
          protocol_fee_num,
          True,
        )
      },
    )
  let n_cases = list.length(deposited_amount_list)
  list.count(validation_results0, fn(r) { r == False }) == n_cases
}

// // =================================== REDEEM TESTS =================================== //
test test_uniform_redeem_with_valid_math() {
  let lp_fee_num = 112
  let protocol_fee_num = 300
  let redeemed_amount_list =
    [1_000_000, 3_000_000_004, 950_040_703_214]
  let a = 231
  let validation_results0 =
    list.map(
      redeemed_amount_list,
      fn(delta) {
        uniform_reserves_valid_lq_math_template(
          n,
          delta,
          a,
          lp_fee_num,
          protocol_fee_num,
          False,
        )
      },
    )
  let n_cases = list.length(redeemed_amount_list)
  list.count(validation_results0, fn(r) { r == True }) == n_cases
}

test test_uniform_redeem_with_invalid_math() {
  let lp_fee_num = 112
  let protocol_fee_num = 300
  let a = 2134
  let redeemed_amount_list =
    [1_000_000, 3_000_000_004, 950_040_703_214]

  let validation_results0 =
    list.map(
      redeemed_amount_list,
      fn(delta) {
        uniform_reserves_invalid_lq_math_template(
          n,
          delta,
          a,
          lp_fee_num,
          protocol_fee_num,
          False,
        )
      },
    )
  let n_cases = list.length(redeemed_amount_list)
  list.count(validation_results0, fn(r) { r == False }) == n_cases
}

// =================================== SWAP TESTS =================================== //
test test_uniform_swap_with_valid_math() {
  // Swap 0 -> 1
  let base_ind0 = 0
  let quote_ind0 = 1
  let protocol_fee_num_list =
    [100, 3000, 20000]
  let base_amount_list =
    [1_000_000_000, 30_000, 3_950_000]
  let swap_fee_list =
    [100, 250, 1000]
  let validation_results0 =
    list.map3(
      base_amount_list,
      swap_fee_list,
      protocol_fee_num_list,
      fn(base, swap_fee, protocol_fee_num) {
        uniform_reserves_valid_swap_math_template(
          n,
          base,
          swap_fee,
          protocol_fee_num,
          base_ind0,
          quote_ind0,
        )
      },
    )

  // Swap 1 -> 0
  let base_ind1 = 1
  let quote_ind1 = 0
  let validation_results1 =
    list.map3(
      base_amount_list,
      swap_fee_list,
      protocol_fee_num_list,
      fn(base, swap_fee, protocol_fee_num) {
        uniform_reserves_valid_swap_math_template(
          n,
          base,
          swap_fee,
          protocol_fee_num,
          base_ind1,
          quote_ind1,
        )
      },
    )

  // Swap 0 -> 2
  let base_ind2 = 0
  let quote_ind2 = 2
  let validation_results2 =
    list.map3(
      base_amount_list,
      swap_fee_list,
      protocol_fee_num_list,
      fn(base, swap_fee, protocol_fee_num) {
        uniform_reserves_valid_swap_math_template(
          n,
          base,
          swap_fee,
          protocol_fee_num,
          base_ind2,
          quote_ind2,
        )
      },
    )

  // Swap 2 -> 0
  let base_ind3 = 2
  let quote_ind3 = 0
  let validation_results3 =
    list.map3(
      base_amount_list,
      swap_fee_list,
      protocol_fee_num_list,
      fn(base, swap_fee, protocol_fee_num) {
        uniform_reserves_valid_swap_math_template(
          n,
          base,
          swap_fee,
          protocol_fee_num,
          base_ind3,
          quote_ind3,
        )
      },
    )

  // Swap 1 -> 2
  let base_ind4 = 1
  let quote_ind4 = 2
  let validation_results4 =
    list.map3(
      base_amount_list,
      swap_fee_list,
      protocol_fee_num_list,
      fn(base, swap_fee, protocol_fee_num) {
        uniform_reserves_valid_swap_math_template(
          n,
          base,
          swap_fee,
          protocol_fee_num,
          base_ind4,
          quote_ind4,
        )
      },
    )

  // Swap 2 -> 1
  let base_ind5 = 2
  let quote_ind5 = 1
  let validation_results5 =
    list.map3(
      base_amount_list,
      swap_fee_list,
      protocol_fee_num_list,
      fn(base, swap_fee, protocol_fee_num) {
        uniform_reserves_valid_swap_math_template(
          n,
          base,
          swap_fee,
          protocol_fee_num,
          base_ind5,
          quote_ind5,
        )
      },
    )

  let n_cases = list.length(base_amount_list)
  list.count(validation_results0, fn(r) { r == True }) == n_cases && list.count(
    validation_results1,
    fn(r) { r == True },
  ) == n_cases && list.count(validation_results2, fn(r) { r == True }) == n_cases && list.count(
    validation_results3,
    fn(r) { r == True },
  ) == n_cases && list.count(validation_results4, fn(r) { r == True }) == n_cases && list.count(
    validation_results5,
    fn(r) { r == True },
  ) == n_cases
}

test test_uniform_swap_with_invalid_math() {
  // Swap 0 -> 1
  let base_ind0 = 0
  let quote_ind0 = 1
  let protocol_fee_num_list =
    [100, 3000, 20000]
  let base_amount_list =
    [1_000_000_000, 30_000, 3_950_000]
  let swap_fee_list =
    [100, 250, 1000]
  let validation_results0 =
    list.map3(
      base_amount_list,
      swap_fee_list,
      protocol_fee_num_list,
      fn(base, swap_fee, protocol_fee_num) {
        uniform_reserves_invalid_swap_math_template(
          n,
          base,
          swap_fee,
          protocol_fee_num,
          base_ind0,
          quote_ind0,
        )
      },
    )

  // Swap 1 -> 0
  let base_ind1 = 1
  let quote_ind1 = 0
  let validation_results1 =
    list.map3(
      base_amount_list,
      swap_fee_list,
      protocol_fee_num_list,
      fn(base, swap_fee, protocol_fee_num) {
        uniform_reserves_invalid_swap_math_template(
          n,
          base,
          swap_fee,
          protocol_fee_num,
          base_ind1,
          quote_ind1,
        )
      },
    )

  // Swap 0 -> 2
  let base_ind2 = 0
  let quote_ind2 = 2
  let validation_results2 =
    list.map3(
      base_amount_list,
      swap_fee_list,
      protocol_fee_num_list,
      fn(base, swap_fee, protocol_fee_num) {
        uniform_reserves_invalid_swap_math_template(
          n,
          base,
          swap_fee,
          protocol_fee_num,
          base_ind2,
          quote_ind2,
        )
      },
    )

  // Swap 2 -> 0
  let base_ind3 = 2
  let quote_ind3 = 0
  let validation_results3 =
    list.map3(
      base_amount_list,
      swap_fee_list,
      protocol_fee_num_list,
      fn(base, swap_fee, protocol_fee_num) {
        uniform_reserves_invalid_swap_math_template(
          n,
          base,
          swap_fee,
          protocol_fee_num,
          base_ind3,
          quote_ind3,
        )
      },
    )

  // Swap 1 -> 2
  let base_ind4 = 1
  let quote_ind4 = 2
  let validation_results4 =
    list.map3(
      base_amount_list,
      swap_fee_list,
      protocol_fee_num_list,
      fn(base, swap_fee, protocol_fee_num) {
        uniform_reserves_invalid_swap_math_template(
          n,
          base,
          swap_fee,
          protocol_fee_num,
          base_ind4,
          quote_ind4,
        )
      },
    )

  // Swap 2 -> 1
  let base_ind5 = 2
  let quote_ind5 = 1
  let validation_results5 =
    list.map3(
      base_amount_list,
      swap_fee_list,
      protocol_fee_num_list,
      fn(base, swap_fee, protocol_fee_num) {
        uniform_reserves_invalid_swap_math_template(
          n,
          base,
          swap_fee,
          protocol_fee_num,
          base_ind5,
          quote_ind5,
        )
      },
    )

  let n_cases = list.length(base_amount_list)
  list.count(validation_results0, fn(r) { r == False }) == n_cases && list.count(
    validation_results1,
    fn(r) { r == False },
  ) == n_cases && list.count(validation_results2, fn(r) { r == False }) == n_cases && list.count(
    validation_results3,
    fn(r) { r == False },
  ) == n_cases && list.count(validation_results4, fn(r) { r == False }) == n_cases && list.count(
    validation_results5,
    fn(r) { r == False },
  ) == n_cases
}

test test_swap_to_max_decimals_fixtures() {
  // 0 -> 1
  let a = 300
  let an2n = a * math.pow(n, 2 * n)
  let lp_fee_num = 20000
  let protocol_fee_num = 50000
  let tradable_assets =
    generate_assets(@"some_pool_token", @"some_pool_token_name", n)
  let tradable_decimals =
    [1_000, 1_000_000, 1_000]
  // Input data:
  let total_balances_before =
    [343088, 475000220, 348093]
  let collected_protocol_fees_before =
    [88088, 220000220, 93093]
  let inv_before = 765000000
  let lp_balance_before = lp_emission - inv_before

  let (input_pool_data, input_value) =
    t2t2t_pool_state(
      total_balances_before,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
    )
  // Output data:
  let total_balances_after =
    [390088, 460901217, 348093]

  let collected_protocol_fees_after =
    [88088, 243498560, 93093]
  let lp_balance_after = lp_balance_before
  let (output_pool_data, output_value) =
    t2t2t_pool_state(
      total_balances_after,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after,
    )
  // Output with error:
  // 1. Wrong balances:
  let total_balances_after_wrong =
    [390088, 428943460, 348092]
  let (_, output_value_wrong_balances) =
    t2t2t_pool_state(
      total_balances_after_wrong,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after,
    )
  // 2. Wrong fees:
  let collected_protocol_fees_after_wrong =
    [88088, 220047215, 93093]
  let (output_pool_data_wrong_fees, _) =
    t2t2t_pool_state(
      total_balances_after,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after_wrong,
    )

  // 3. Wrong invariant:
  let inv_before_wrong = inv_before - 1

  // Validations:
  validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data,
    [inv_before],
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value_wrong_balances,
    output_pool_data,
    [inv_before],
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_fees,
    [inv_before],
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data,
    [inv_before_wrong],
  )
}

test test_swap_to_min_decimals_fixtures() {
  // 0 -> 1
  let a = 300
  let an2n = a * math.pow(n, 2 * n)
  let lp_fee_num = 20000
  let protocol_fee_num = 50000
  let tradable_assets =
    generate_assets(@"some_pool_token", @"some_pool_token_name", n)
  let tradable_decimals =
    [1_000_000, 1_000, 1_000]
  // Input data:
  let total_balances_before =
    [475000220, 343088, 348093]
  let collected_protocol_fees_before =
    [220000220, 88088, 93093]
  let inv_before = 765000000
  let lp_balance_before = lp_emission - inv_before

  let (input_pool_data, input_value) =
    t2t2t_pool_state(
      total_balances_before,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
    )
  // Output data:
  let total_balances_after =
    [468863264, 390088, 348093]

  let collected_protocol_fees_after =
    [230228480, 88088, 93093]
  let lp_balance_after = lp_balance_before
  let (output_pool_data, output_value) =
    t2t2t_pool_state(
      total_balances_after,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after,
    )
  // Output with error:
  // 1. Wrong balances:
  let total_balances_after_wrong =
    [390088, 428943460, 348092]
  let (_, output_value_wrong_balances) =
    t2t2t_pool_state(
      total_balances_after_wrong,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after,
    )
  // 2. Wrong fees:
  let collected_protocol_fees_after_wrong =
    [88088, 220047215, 93093]
  let (output_pool_data_wrong_fees, _) =
    t2t2t_pool_state(
      total_balances_after,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after_wrong,
    )

  // 3. Wrong invariant:
  let inv_before_wrong = inv_before - 1

  // Validations:
  validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data,
    [inv_before],
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value_wrong_balances,
    output_pool_data,
    [inv_before],
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_fees,
    [inv_before],
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data,
    [inv_before_wrong],
  )
}

// ================================================= SWAP COST TEST ================================================= //

test single_swap_test() {
  let base_amount = 1000000
  let lp_fee_num_before = 10012
  let protocol_fee_num_before = 325
  let min_value = 1_000_000
  let default_decimal = 1_000_000
  let base_ind = 0
  let quote_ind = 1
  let default_token_value = min_value * default_decimal

  let tradable_assets =
    generate_assets(@"some_pool_token", @"some_pool_token_name", n)

  // Input data:
  let total_balances_before = list.repeat(default_token_value, n)
  let tradable_decimals_before = list.repeat(default_decimal, n)
  let collected_protocol_fees_before = list.repeat(0, n)
  let a_before = 1000
  let an2n = a_before * math.pow(n, 2 * n)

  let inv_before = default_token_value * n
  let lp_balance_before = lp_emission - inv_before

  let (input_pool_data, input_value) =
    t2t2t_pool_state(
      total_balances_before,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
    )
  // Output data:
  let quote_lp_fees = base_amount * input_pool_data.lp_fee_num / denom
  let quote_protocol_fees =
    base_amount * input_pool_data.protocol_fee_num / denom

  let total_balances_after =
    list.indexed_map(
      total_balances_before,
      fn(i, b) {
        if i == base_ind {
          b + base_amount
        } else if i == quote_ind {
          b - base_amount + quote_lp_fees + quote_protocol_fees + 1
        } else {
          b
        }
      },
    )
  let collected_protocol_fees_after =
    list.indexed_map(
      collected_protocol_fees_before,
      fn(i, b) {
        if i == quote_ind {
          b + quote_protocol_fees
        } else {
          b
        }
      },
    )

  let (output_pool_data, output_value) =
    t2t2t_pool_state(
      total_balances_after,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after,
    )

  let ctx =
    make_t2t2t_pool_state_transition_context(
      input_pool_data,
      input_value,
      output_pool_data,
      output_value,
    )
  let redeemer = PoolRedeemer(0, 0, AMMAction([inv_before]))
  validate_stable_pool_transition_t2t2t(input_pool_data, redeemer, ctx)
}

test empty_swap_test() {
  let base_amount = 1000000
  let lp_fee_num_before = 10012
  let protocol_fee_num_before = 325
  let min_value = 1_000_000
  let default_decimal = 1_000_000
  let base_ind = 0
  let quote_ind = 1
  let default_token_value = min_value * default_decimal

  let tradable_assets =
    generate_assets(@"some_pool_token", @"some_pool_token_name", n)

  // Input data:
  let total_balances_before = list.repeat(default_token_value, n)
  let tradable_decimals_before = list.repeat(default_decimal, n)
  let collected_protocol_fees_before = list.repeat(0, n)
  let a_before = 1000
  let an2n = a_before * math.pow(n, 2 * n)

  let inv_before = default_token_value * n
  let lp_balance_before = lp_emission - inv_before

  let (input_pool_data, input_value) =
    t2t2t_pool_state(
      total_balances_before,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
    )
  // Output data:
  let quote_lp_fees = base_amount * input_pool_data.lp_fee_num / denom
  let quote_protocol_fees =
    base_amount * input_pool_data.protocol_fee_num / denom

  let total_balances_after =
    list.indexed_map(
      total_balances_before,
      fn(i, b) {
        if i == base_ind {
          b + base_amount
        } else if i == quote_ind {
          b - base_amount + quote_lp_fees + quote_protocol_fees + 1
        } else {
          b
        }
      },
    )
  let collected_protocol_fees_after =
    list.indexed_map(
      collected_protocol_fees_before,
      fn(i, b) {
        if i == quote_ind {
          b + quote_protocol_fees
        } else {
          b
        }
      },
    )

  let (output_pool_data, output_value) =
    t2t2t_pool_state(
      total_balances_after,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      an2n,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after,
    )

  let ctx =
    make_t2t2t_pool_state_transition_context(
      input_pool_data,
      input_value,
      output_pool_data,
      output_value,
    )
  let redeemer = PoolRedeemer(0, 0, AMMAction([inv_before]))
  ctx == ctx && redeemer == redeemer
}
