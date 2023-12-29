use aiken/list
use aiken/math.{max}
use aiken/transaction.{
  Input, Output, ScriptContext, Spend, Transaction, find_datum, find_input,
}
use aiken/transaction/value.{quantity_of}
use whalepools/plutus.{Asset}
use whalepools/stable3pool/types.{PoolData}
use whalepools/stable3pool/utils.{
  check_invariant_extremum, check_invariant_extremum_for_asset,
  convert_pool_datum,
}
use whalepools/tests/stable3pool/templates.{
  PoolTestAction, pool_amm_action_state_transition_tx,
  pool_data_with_default_config, pool_value,
}

// Number of tradable assets in the pool:
const n = 3

// Value of n^n:
const nn = 27

// Common denominator of calculations:
const denom = 10_000

// LP tokens emisson:
const lp_emission = 9223372036854775807

fn stable3_pool(action: PoolTestAction, ctx: ScriptContext) -> Bool {
  let ScriptContext { transaction, purpose } = ctx
  expect Spend(spent_utxo_reference) = purpose
  // ==================================== POOL INPUT ==================================== //
  expect Some(self_input) = find_input(transaction.inputs, spent_utxo_reference)
  let self = self_input.output
  let datum = action.input_data

  let script0 = self.reference_script
  // Pool's tokens | Input:
  let pool_nft0 = datum.pool_nft
  let base_token0 = datum.base_token
  let quote0_token0 = datum.quote0_token
  let quote1_token0 = datum.quote1_token
  let lp_token0 = datum.lp_token

  // Decimal values ​​of tradable assets | Input:
  let base_dec0 = datum.base_dec
  let quote0_dec0 = datum.quote0_dec
  let quote1_dec0 = datum.quote1_dec

  // Treasury & DAO | Input:
  let treasury_addr0 = datum.treasury_address
  let dao_policy0 = datum.dao_policy

  // Actual balance of collected protocol fees (accumulated in the pool) | Input:
  let base_protocol_fees0 = datum.base_protocol_fees
  let quote0_protocol_fees0 = datum.quote0_protocol_fees
  let quote1_protocol_fees0 = datum.quote1_protocol_fees

  // Amplification coefficient of the StableSwap invariant | Input:
  let a0 = datum.ampl_coeff

  // Numerators of fees | Input:
  let swap_fee_num0 = datum.swap_fee_num
  let protocol_share_num0 = datum.protocol_share_num

  // Values of the invariant | Input:
  let inv_native0 = datum.inv_native
  let inv0 = datum.inv

  // ==================================== POOL OUTPUT ==================================== //
  expect Some(successor) = list.at(transaction.outputs, action.successor_ix)
  expect Some(successor_data) =
    find_datum(
      outputs: transaction.outputs,
      datums: transaction.datums,
      datum_hash: action.successor_data_hash,
    )
  let datum_successor = convert_pool_datum(successor_data)
  let script1 = successor.reference_script
  // Pool's tokens | Output:
  let pool_nft1 = datum_successor.pool_nft
  let base_token1 = datum_successor.base_token
  let quote0_token1 = datum_successor.quote0_token
  let quote1_token1 = datum_successor.quote1_token
  let lp_token1 = datum_successor.lp_token

  // Decimal values ​​of tradable assets | Output:
  let base_dec1 = datum_successor.base_dec
  let quote0_dec1 = datum_successor.quote0_dec
  let quote1_dec1 = datum_successor.quote1_dec

  // Treasury & DAO | Output:
  let treasury_addr1 = datum_successor.treasury_address
  let dao_policy1 = datum_successor.dao_policy

  // Actual balance of collected protocol fees (accumulated in the pool) | Output:
  let base_protocol_fees1 = datum_successor.base_protocol_fees
  let quote0_protocol_fees1 = datum_successor.quote0_protocol_fees
  let quote1_protocol_fees1 = datum_successor.quote1_protocol_fees

  // Amplification coefficient of the StableSwap invariant | Output:
  let a1 = datum_successor.ampl_coeff

  // Numerators of fees | Output:
  let swap_fee_num1 = datum_successor.swap_fee_num
  let protocol_share_num1 = datum_successor.protocol_share_num

  // Values of the invariant | Output:
  let inv_native1 = datum_successor.inv_native
  let inv1 = datum_successor.inv

  // ==================================== DELTAS ==================================== //
  // Pool's tokens policies:
  let pool_nft_policy = pool_nft0.policy
  let base_policy = base_token0.policy
  let quote0_policy = quote0_token0.policy
  let quote1_policy = quote1_token0.policy
  let lp_policy = lp_token0.policy

  // We do not take into account the current protocol fees for deposit/swap/redeem actions!
  let base_amount_total0 =
    quantity_of(self.value, base_policy, base_token0.name)
  let base_amount0 = base_amount_total0 - base_protocol_fees0

  let base_amount_total1 =
    quantity_of(successor.value, base_policy, base_token0.name)
  let base_amount1 = base_amount_total1 - base_protocol_fees1

  let quote0_amount_total0 =
    quantity_of(self.value, quote0_policy, quote0_token0.name)
  let quote0_amount0 = quote0_amount_total0 - quote0_protocol_fees0

  let quote0_amount_total1 =
    quantity_of(successor.value, quote0_policy, quote0_token0.name)
  let quote0_amount1 = quote0_amount_total1 - quote0_protocol_fees1

  let quote1_amount_total0 =
    quantity_of(self.value, quote1_policy, quote1_token0.name)
  let quote1_amount0 = quote1_amount_total0 - quote1_protocol_fees0

  let quote1_amount_total1 =
    quantity_of(successor.value, quote1_policy, quote1_token1.name)
  let quote1_amount1 = quote1_amount_total1 - quote1_protocol_fees1

  let lp_amount0 = quantity_of(self.value, lp_policy, lp_token0.name)
  let lp_amount1 = quantity_of(successor.value, lp_policy, lp_token0.name)

  // Calculating tradable assets deltas:
  let delta_base = base_amount1 - base_amount0
  let delta_quote0 = quote0_amount1 - quote0_amount0
  let delta_quote1 = quote1_amount1 - quote1_amount0
  let delta_lp = lp_amount1 - lp_amount0

  // Collected protocol fees deltas:
  let delta_base_protocol_fees = base_protocol_fees1 - base_protocol_fees0
  let delta_quote0_protocol_fees = quote0_protocol_fees1 - quote0_protocol_fees0
  let delta_quote1_protocol_fees = quote1_protocol_fees1 - quote1_protocol_fees0

  // ==================================== VALIDATIONS ==================================== //
  // 1. Pool NFT is preserved:
  let pool_nft_token_preserved =
    quantity_of(self.value, pool_nft_policy, pool_nft0.name) == 1 && quantity_of(
      successor.value,
      pool_nft_policy,
      pool_nft0.name,
    ) == 1

  // 2. Immutable pool configuration parameters are preserved:
  let pool_nft_preserved = pool_nft0 == pool_nft1
  let tradeble_tokens_preserved =
    base_token0 == base_token1 && quote0_token0 == quote0_token1 && quote1_token0 == quote1_token1 && lp_token0 == lp_token1
  let config_tokens_decimals_preserved =
    base_dec0 == base_dec1 && quote0_dec0 == quote0_dec1 && quote1_dec0 == quote1_dec1

  let preserved_config =
    pool_nft_preserved && tradeble_tokens_preserved && config_tokens_decimals_preserved

  // 3. Validator script is preserved:
  let preserved_script = script0 == script1

  // 4. One of the mutable parameter is not preserved || protocol fees withdraw, i.e. checking if action must be validated by the DAO contract:
  let voting_params_preserved =
    treasury_addr0 == treasury_addr1 && dao_policy0 == dao_policy1 && a0 == a1 && swap_fee_num0 == swap_fee_num1 && protocol_share_num0 == protocol_share_num1
  let protocol_fees_not_decreased =
    delta_base_protocol_fees >= 0 && delta_quote0_protocol_fees >= 0 && delta_quote1_protocol_fees >= 0
  let dao_action = !(voting_params_preserved && protocol_fees_not_decreased)
  // 5. Action is valid:
  let valid_action =
    if dao_action {
      // 5.1. DAO action. #TODO.
      False
    } else {
      // Precision multiplier for calculations:
      let precision = max(max(base_dec0, quote0_dec0), quote1_dec0)
      // Widely used constant:
      let ann = a0 * nn

      // Calculate final amounts without latest fees:
      let base_amount1_no_fees = base_amount_total1 - base_protocol_fees0
      let quote0_amount1_no_fees = quote0_amount_total1 - quote0_protocol_fees0
      let quote1_amount1_no_fees = quote1_amount_total1 - quote1_protocol_fees0

      // Convert all amounts to the equal precision:
      let base_amount1_calc = precision * base_amount1 / base_dec0
      let quote0_amount1_calc = precision * quote0_amount1 / quote0_dec0
      let quote1_amount1_calc = precision * quote1_amount1 / quote1_dec0

      let base_amount1_no_fees_calc =
        precision * base_amount1_no_fees / base_dec0
      let quote0_amount1_no_fees_calc =
        precision * quote0_amount1_no_fees / quote0_dec0
      let quote1_amount1_no_fees_calc =
        precision * quote1_amount1_no_fees / quote1_dec0

      if delta_lp != 0 {
        // 5.2. Deposit || Redeem

        // Check the invariant.
        // Invariant is valid if there is an extremum (min) with the final tradable assets amounts substituted.
        let valid_invariant_extremum: Bool =
          check_invariant_extremum(
            x_calc: base_amount1_calc,
            y_calc: quote0_amount1_calc,
            z_calc: quote1_amount1_calc,
            d: inv1,
            nn: nn,
            ann: ann,
          )

        // Check the native invariant.
        // Native invariant is valid if there is an extremum (min) with the tradable assets amounts without latest fees substituted.
        let valid_native_invariant_extremum: Bool =
          check_invariant_extremum(
            x_calc: base_amount1_no_fees_calc,
            y_calc: quote0_amount1_no_fees_calc,
            z_calc: quote1_amount1_no_fees_calc,
            d: inv_native1,
            nn: nn,
            ann: ann,
          )

        // Calculate LP tokens delta:
        let supply_lp0 = lp_emission - lp_amount0
        let delta_supply_lp = ( inv1 - inv0 ) * supply_lp0 / inv0

        // LP tokens delta is valid:
        let valid_lp_delta = -delta_lp == delta_supply_lp

        // Calculate ideal amounts (uniform deltas case):
        let base_ideal = base_amount0 * inv_native1 / inv0
        let quote0_ideal = quote0_amount0 * inv_native1 / inv0
        let quote1_ideal = quote1_amount0 * inv_native1 / inv0

        // Calculate difference from the ideal amounts:
        let base_diff =
          if base_ideal > base_amount1_no_fees {
            base_ideal - base_amount1_no_fees
          } else {
            base_amount1_no_fees - base_ideal
          }
        let quote0_diff =
          if quote0_ideal > quote0_amount1_no_fees {
            quote0_ideal - quote0_amount1_no_fees
          } else {
            quote0_amount1_no_fees - quote0_ideal
          }
        let quote1_diff =
          if quote1_ideal > quote1_amount1_no_fees {
            quote1_ideal - quote1_amount1_no_fees
          } else {
            quote1_amount1_no_fees - quote1_ideal
          }

        // Calculate fees:
        let fee_num = swap_fee_num0 * n / ( 4 * ( n - 1 ) )

        let protocol_fees_base =
          fee_num * base_diff * protocol_share_num0 / ( denom * denom )
        let protocol_fees_quote0 =
          fee_num * quote0_diff * protocol_share_num0 / ( denom * denom )
        let protocol_fees_quote1 =
          fee_num * quote1_diff * protocol_share_num0 / ( denom * denom )
        let valid_protocol_fees =
          delta_base_protocol_fees == protocol_fees_base && delta_quote0_protocol_fees == protocol_fees_quote0 && delta_quote1_protocol_fees == protocol_fees_quote1

        // Valid invariant change:
        let valid_invariant_movement =
          if delta_lp > 0 {
            inv1 < inv0 && inv_native1 < inv_native0
          } else {
            inv1 > inv0 && inv_native1 > inv_native0
          }

        valid_invariant_extremum && valid_native_invariant_extremum && valid_protocol_fees && valid_lp_delta && valid_invariant_movement
      } else {
        // 5.3. Swap

        // LP tokens amount is preserved:
        let valid_lp_delta_for_swap = delta_lp == 0

        // Difference between invariant of the previous state and current native invariant:
        let inv_diff =
          if inv_native1 > inv0 {
            inv_native1 - inv0
          } else {
            inv0 - inv_native1
          }

        // Basic validations:
        let (
          quote_asset_ind,
          base_amount1_no_fees_calc_for_swap,
          quote0_amount1_no_fees_calc_for_swap,
          quote1_amount1_no_fees_calc_for_swap,
          minimal_quote_asset_unit,
          valid_quote_swap_deltas,
        ) =
          // Swap Y/Z to X:
          if delta_base < 0 {
            let min_base_unit = precision / base_dec0
            (
              // Index of the quote asset:
              0,
              // Base asset without latest fees:
              base_amount1_no_fees_calc - delta_base_protocol_fees - delta_base_protocol_fees * (
                denom - protocol_share_num0
              ) / protocol_share_num0,
              // First quote asset without latest fees:
              quote0_amount1_no_fees_calc,
              // Second quote asset without latest fees:
              quote1_amount1_no_fees_calc,
              // Minimal logical step of the quote asset change:
              min_base_unit,
              // Valid deltas:
              if delta_quote0 > 0 {
                delta_quote1 == 0
              } else {
                delta_quote1 > 0 && delta_quote0 == 0
              },
            )
          } else if // Value 'inv_diff' should not exeed 2 minimal quote asset units:
          // Swap X/Z to Y:
          delta_quote0 < 0 {
            let min_quote0_unit = precision / quote0_dec0
            (
              // Index of the quote asset:
              1,
              // Base asset without latest fees:
              base_amount1_no_fees_calc,
              // First quote asset without latest fees:
              quote0_amount1_no_fees_calc - delta_quote0_protocol_fees - delta_quote0_protocol_fees * (
                denom - protocol_share_num0
              ) / protocol_share_num0,
              // Second quote asset without latest fees:
              quote1_amount1_no_fees_calc,
              // Minimal logical step of the quote asset change:
              min_quote0_unit,
              // Valid deltas:
              if delta_base > 0 {
                delta_quote1 == 0
              } else {
                delta_quote1 > 0 && delta_base == 0
              },
            )
          } else if // Swap X/Y to Z:
          delta_quote1 < 0 {
            let min_quote1_unit = precision / quote1_dec0

            (
              // Index of the quote asset:
              2,
              // Base asset without latest fees:
              base_amount1_no_fees_calc,
              // First quote asset without latest fees:
              quote0_amount1_no_fees_calc,
              // Second quote asset without latest fees:
              quote1_amount1_no_fees_calc - delta_quote1_protocol_fees - delta_quote1_protocol_fees * (
                denom - protocol_share_num0
              ) / protocol_share_num0,
              // Minimal logical step of the quote asset change:
              min_quote1_unit,
              // Valid deltas:
              if delta_base > 0 {
                delta_quote0 == 0
              } else {
                delta_quote0 > 0 && delta_base == 0
              },
            )
          } else {
            (0, 0, 0, 0, 0, False)
          }

        // Value 'inv_diff' should not exeed 2 minimal quote asset units:
        let native_invariant_preserved =
          inv_diff <= 2 * minimal_quote_asset_unit
        // let 
        // Check the invariant for swap:
        let valid_invariant_extremum_for_asset: Bool =
          check_invariant_extremum_for_asset(
            x_calc: base_amount1_calc,
            y_calc: quote0_amount1_calc,
            z_calc: quote1_amount1_calc,
            d: inv1,
            nn: nn,
            ann: ann,
            minimal_asset_unit: minimal_quote_asset_unit,
            asset_ind: quote_asset_ind,
          )

        // Check the native invariant for swap:
        let valid_native_invariant_extremum_for_asset: Bool =
          check_invariant_extremum_for_asset(
            x_calc: base_amount1_no_fees_calc_for_swap,
            y_calc: quote0_amount1_no_fees_calc_for_swap,
            z_calc: quote1_amount1_no_fees_calc_for_swap,
            d: inv_native1,
            nn: nn,
            ann: ann,
            minimal_asset_unit: minimal_quote_asset_unit,
            asset_ind: quote_asset_ind,
          )
        valid_invariant_extremum_for_asset && valid_native_invariant_extremum_for_asset && native_invariant_preserved && valid_lp_delta_for_swap && valid_quote_swap_deltas
      }
    }
  pool_nft_token_preserved && preserved_config && preserved_script && valid_action
}

test test_stable3pool_deposit() {
  // Input data:
  let input_data =
    pool_data_with_default_config(
      base_protocol_fees: 3922600039226,
      quote0_protocol_fees: 1989919899,
      quote1_protocol_fees: 31614583,
      inv_native: 249586736899936,
      inv: 249586736899936,
    )

  let input_value =
    pool_value(
      base_amount: 10476100039226,
      quote0_amount: 8543419899,
      quote1_amount: 97149583,
      lp_amount: 9223343302408446571,
    )
  // Output data:
  let output_data =
    pool_data_with_default_config(
      base_protocol_fees: 4038414485117,
      quote0_protocol_fees: 229707313770,
      quote1_protocol_fees: 1279855697,
      inv_native: 12071847959036641,
      inv: 11954682477020570,
    )
  let output_value =
    pool_value(
      base_amount: 12059477004396,
      quote0_amount: 607565348976097,
      quote1_amount: 3331844339354,
      lp_amount: 9221995716997856785,
    )

  // Deposit Action:
  let (context, action) =
    pool_amm_action_state_transition_tx(
      input_data,
      input_value,
      output_data,
      output_value,
    )

  stable3_pool(action, context)
}

test test_stable3pool_redeem() {
  // Input data:
  let input_data =
    pool_data_with_default_config(
      base_protocol_fees: 2348600023486,
      quote0_protocol_fees: 4257842578,
      quote1_protocol_fees: 9889880,
      inv_native: 249586736899936,
      inv: 249586736899936,
    )

  let input_value =
    pool_value(
      base_amount: 8902100023486,
      quote0_amount: 10811342578,
      quote1_amount: 75424880,
      lp_amount: 9223344781018733046,
    )
  // Output data:
  let output_data =
    pool_data_with_default_config(
      base_protocol_fees: 2348614670947,
      quote0_protocol_fees: 4257884180,
      quote1_protocol_fees: 9890002,
      inv_native: 248099155340753,
      inv: 248098361891813,
    )
  let output_value =
    pool_value(
      base_amount: 8902099890353,
      quote0_amount: 10661342428,
      quote1_amount: 75359815,
      lp_amount: 9223344943555034836,
    )

  // Redeem Action:
  let (context, action) =
    pool_amm_action_state_transition_tx(
      input_data,
      input_value,
      output_data,
      output_value,
    )

  stable3_pool(action, context)
}

test test_stable3pool_swap() {
  // Input data:
  let input_data =
    pool_data_with_default_config(
      base_protocol_fees: 88088,
      quote0_protocol_fees: 220000220,
      quote1_protocol_fees: 93093,
      inv_native: 765000000,
      inv: 765000000,
    )
  let input_value =
    pool_value(
      base_amount: 343088,
      quote0_amount: 475000220,
      quote1_amount: 348093,
      lp_amount: 9223372035688599693,
    )
  // Output data:
  let output_data =
    pool_data_with_default_config(
      base_protocol_fees: 88088,
      quote0_protocol_fees: 220047216,
      quote1_protocol_fees: 93093,
      inv_native: 765000000,
      inv: 765892997,
    )
  let output_value =
    pool_value(
      base_amount: 390088,
      quote0_amount: 428943460,
      quote1_amount: 348093,
      lp_amount: 9223372035688599693,
    )

  // Swap Action:
  let (context, action) =
    pool_amm_action_state_transition_tx(
      input_data,
      input_value,
      output_data,
      output_value,
    )

  stable3_pool(action, context)
}
