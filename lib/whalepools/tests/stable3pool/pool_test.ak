use aiken/list
use aiken/math.{max, min}
use aiken/transaction.{
  Input, Output, ScriptContext, Spend, Transaction, find_datum, find_input,
}
use aiken/transaction/value.{quantity_of}
use whalepools/plutus.{Asset}
use whalepools/stable3pool/types.{PoolAction, PoolData}
use whalepools/stable3pool/utils.{check_invariant_extremum, convert_pool_datum}
use whalepools/tests/stable3pool/templates.{
  pool_amm_action_state_transition_tx, pool_data_with_default_config, pool_value,
}

// Number of tradable assets in the pool:
const n = 3

// Value of n^n:
const nn = 27

// Common denominator of calculations:
const denom = 10_000

// LP tokens emisson:
const lp_emission = 9223372036854775807

fn stable3_pool(action: PoolAction, ctx: ScriptContext) -> Bool {
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

  // We do not take into account the current protocol commissions for deposit/swap/redeem actions!
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
  // Precision multiplier for calculations:
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
      // In the case of a swap, the invariants are swapped, since without commissions the amount of tradable assets are preserved:
      let inv1_calc =
        if delta_lp == 0 {
          inv_native1
        } else {
          inv1
        }
      let inv_native1_calc =
        if delta_lp == 0 {
          inv1
        } else {
          inv_native1
        }

      let precision = max(max(base_dec0, quote0_dec0), quote1_dec0)

      let base_amount1_calc = precision * base_amount1 / base_dec0
      let quote0_amount1_calc = precision * quote0_amount1 / quote0_dec0
      let quote1_amount1_calc = precision * quote1_amount1 / quote1_dec0

      // Check the invariant.
      // Invariant is valid if there is an extremum (min) with the final tradable assets amounts substituted.
      let ann = a0 * nn
      let valid_invariant_extremum: Bool =
        check_invariant_extremum(
          x_calc: base_amount1_calc,
          y_calc: quote0_amount1_calc,
          z_calc: quote1_amount1_calc,
          d: inv1_calc,
          nn: nn,
          ann: ann,
        )

      // Calculate final amounts without latest fees:
      let base_amount1_no_fees = base_amount_total1 - base_protocol_fees0
      let quote0_amount1_no_fees = quote0_amount_total1 - quote0_protocol_fees0
      let quote1_amount1_no_fees = quote1_amount_total1 - quote1_protocol_fees0

      // Convert all amounts to the common precision:
      let base_amount1_no_fees_calc =
        precision * base_amount1_no_fees / base_dec0
      let quote0_amount1_no_fees_calc =
        precision * quote0_amount1_no_fees / quote0_dec0
      let quote1_amount1_no_fees_calc =
        precision * quote1_amount1_no_fees / quote1_dec0

      // Check the native invariant.
      // Native invariant is valid if there is an extremum (min) with the tradable assets amounts without latest fees substituted.
      let valid_native_invariant_extremum: Bool =
        check_invariant_extremum(
          x_calc: base_amount1_no_fees_calc,
          y_calc: quote0_amount1_no_fees_calc,
          z_calc: quote1_amount1_no_fees_calc,
          d: inv_native1_calc,
          nn: nn,
          ann: ann,
        )

      if delta_lp != 0 {
        // 5.2. Deposit || Redeem

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
        // Invariant value is preserved:
        let min_decimals = min(min(base_dec0, quote0_dec0), quote1_dec0)
        let max_inv_unit_errror = n * precision / min_decimals
        let abs_inv_diff =
          if inv1_calc > inv0 {
            inv1_calc - inv0
          } else {
            inv0 - inv1_calc
          }
        let valid_invariant_for_swap = abs_inv_diff <= max_inv_unit_errror

        // LP tokens amount is preserved:
        let valid_lp_delta_for_swap = delta_lp == 0

        let (valid_protocol_swap_fees, valid_quote_swap_deltas) =
          // Swap Y/Z to X:
          if delta_base < 0 {
            (
              delta_base_protocol_fees == -delta_base * swap_fee_num0 * protocol_share_num0 / (
                denom * denom
              ),
              // While swapping Y to X, delta_z must be 0 and delta_y must be > 0, etc.:
              if delta_quote0 > 0 {
                delta_quote1 == 0
              } else {
                delta_quote1 > 0 && delta_quote0 == 0
              },
            )
          } else if // Swap X/Z to Y:
          delta_quote0 < 0 {
            (
              delta_quote0_protocol_fees == -delta_quote0 * swap_fee_num0 * protocol_share_num0 / (
                denom * denom
              ),
              if delta_base > 0 {
                delta_quote1 == 0
              } else {
                delta_quote1 > 0 && delta_base == 0
              },
            )
          } else if // Swap X/Y to Z:
          delta_quote1 < 0 {
            (
              delta_quote1_protocol_fees == -delta_quote1 * swap_fee_num0 * protocol_share_num0 / (
                denom * denom
              ),
              if delta_base > 0 {
                delta_quote0 == 0
              } else {
                delta_quote0 > 0 && delta_base == 0
              },
            )
          } else {
            (False, False)
          }

        valid_invariant_extremum && valid_native_invariant_extremum && valid_invariant_for_swap && valid_lp_delta_for_swap && valid_protocol_swap_fees && valid_quote_swap_deltas
      }
    }
  pool_nft_token_preserved && preserved_config && preserved_script && valid_action
}

test test_stable3pool_deposit() {
  // Empty pool config:
  let empty_pool_config =
    pool_data_with_default_config(
      base_protocol_fees: 0,
      quote0_protocol_fees: 0,
      quote1_protocol_fees: 0,
      inv_native: 0,
      inv: 0,
    )

  // Input data:
  let usd_value = 1_000
  let x0 = usd_value * empty_pool_config.base_dec
  let y0 = usd_value * empty_pool_config.quote0_dec
  let z0 = usd_value * empty_pool_config.quote1_dec
  let sum_usd0 = 3 * usd_value
  let precision =
    max(
      max(empty_pool_config.base_dec, empty_pool_config.quote0_dec),
      empty_pool_config.quote1_dec,
    )
  let supply_lp0 = sum_usd0 * precision

  let input_data =
    pool_data_with_default_config(
      base_protocol_fees: 0,
      quote0_protocol_fees: 0,
      quote1_protocol_fees: 0,
      inv_native: supply_lp0,
      inv: supply_lp0,
    )

  let input_value =
    pool_value(
      base_amount: x0,
      quote0_amount: y0,
      quote1_amount: z0,
      lp_amount: lp_emission - supply_lp0,
    )
  // Output data:
  let usd_value = 2_000
  let x1 = usd_value * empty_pool_config.base_dec
  let y1 = usd_value * empty_pool_config.quote0_dec
  let z1 = usd_value * empty_pool_config.quote1_dec
  let sum_usd1 = 3 * usd_value
  let supply_lp1 = sum_usd1 * precision
  let output_data =
    pool_data_with_default_config(
      base_protocol_fees: 0,
      quote0_protocol_fees: 0,
      quote1_protocol_fees: 0,
      inv_native: supply_lp1,
      inv: supply_lp1,
    )
  let output_value =
    pool_value(
      base_amount: x1,
      quote0_amount: y1,
      quote1_amount: z1,
      lp_amount: lp_emission - supply_lp1,
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

test test_stable3pool_imbalanced_deposit() {
  // Empty pool config:
  let empty_pool_config =
    pool_data_with_default_config(
      base_protocol_fees: 0,
      quote0_protocol_fees: 0,
      quote1_protocol_fees: 0,
      inv_native: 0,
      inv: 0,
    )

  // Input data:
  let usd_value = 1_000
  let x0 = usd_value * empty_pool_config.base_dec
  let y0 = usd_value * empty_pool_config.quote0_dec
  let z0 = usd_value * empty_pool_config.quote1_dec
  let sum0 = 3 * usd_value
  let precision =
    max(
      max(empty_pool_config.base_dec, empty_pool_config.quote0_dec),
      empty_pool_config.quote1_dec,
    )
  let supply_lp0 = sum0 * precision

  let input_data =
    pool_data_with_default_config(
      base_protocol_fees: 0,
      quote0_protocol_fees: 0,
      quote1_protocol_fees: 0,
      inv_native: supply_lp0,
      inv: supply_lp0,
    )

  let input_value =
    pool_value(
      base_amount: x0,
      quote0_amount: y0,
      quote1_amount: z0,
      lp_amount: lp_emission - supply_lp0,
    )
  // Output data:
  let usd_value = 10_000
  let x1 = x0
  let y1 = y0 + usd_value * empty_pool_config.quote0_dec
  let z1 = z0
  let inv1 = 12989768785
  let inv2 = 12984756899
  let output_data =
    pool_data_with_default_config(
      base_protocol_fees: 1248,
      quote0_protocol_fees: 2501278,
      quote1_protocol_fees: 1248,
      inv_native: inv1,
      inv: inv2,
    )
  let output_value =
    pool_value(
      base_amount: x1,
      quote0_amount: y1,
      quote1_amount: z1,
      lp_amount: 9223372023870018908,
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
  // Empty pool config:
  let empty_pool_config =
    pool_data_with_default_config(
      base_protocol_fees: 0,
      quote0_protocol_fees: 0,
      quote1_protocol_fees: 0,
      inv_native: 0,
      inv: 0,
    )

  // Input data:
  let usd_value = 1_000
  let x0 = usd_value * empty_pool_config.base_dec
  let y0 = usd_value * empty_pool_config.quote0_dec
  let z0 = usd_value * empty_pool_config.quote1_dec
  let sum0 = 3 * usd_value
  let precision =
    max(
      max(empty_pool_config.base_dec, empty_pool_config.quote0_dec),
      empty_pool_config.quote1_dec,
    )

  let supply_lp0 = sum0 * precision

  let input_data =
    pool_data_with_default_config(
      base_protocol_fees: 0,
      quote0_protocol_fees: 0,
      quote1_protocol_fees: 0,
      inv_native: supply_lp0,
      inv: supply_lp0,
    )

  let input_value =
    pool_value(
      base_amount: x0,
      quote0_amount: y0,
      quote1_amount: z0,
      lp_amount: lp_emission - supply_lp0,
    )
  // Output data:
  let usd_value = 500
  let x1 = usd_value * empty_pool_config.base_dec
  let y1 = usd_value * empty_pool_config.quote0_dec
  let z1 = usd_value * empty_pool_config.quote1_dec
  let sum1 = 3 * usd_value
  let precision =
    max(
      max(empty_pool_config.base_dec, empty_pool_config.quote0_dec),
      empty_pool_config.quote1_dec,
    )

  let supply_lp1 = sum1 * precision
  let output_data =
    pool_data_with_default_config(
      base_protocol_fees: 0,
      quote0_protocol_fees: 0,
      quote1_protocol_fees: 0,
      inv_native: supply_lp1,
      inv: supply_lp1,
    )
  let output_value =
    pool_value(
      base_amount: x1,
      quote0_amount: y1,
      quote1_amount: z1,
      lp_amount: lp_emission - supply_lp1,
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

test test_stable3pool_imbalanced_redeem() {
  // Input data:
  let x0 = 1000000
  let y0 = 11000000000
  let z0 = 1000000

  let input_data =
    pool_data_with_default_config(
      base_protocol_fees: 1248,
      quote0_protocol_fees: 2501278,
      quote1_protocol_fees: 1248,
      inv_native: 12984756899,
      inv: 12984756899,
    )

  let input_value =
    pool_value(
      base_amount: x0,
      quote0_amount: y0,
      quote1_amount: z0,
      lp_amount: 9223372023870018908,
    )

  // Output data:
  let x1 = 500000
  let y1 = 6000000000
  let z1 = 1000000
  let inv1 = 7491107818
  let inv2 = 7490790275
  let output_data =
    pool_data_with_default_config(
      base_protocol_fees: 1277,
      quote0_protocol_fees: 2631451,
      quote1_protocol_fees: 1406,
      inv_native: inv1,
      inv: inv2,
    )
  let output_value =
    pool_value(
      base_amount: x1,
      quote0_amount: y1,
      quote1_amount: z1,
      lp_amount: 9223372029363985532,
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
  let x0 = 1000000
  let y0 = 11000000000
  let z0 = 1000000
  let input_data =
    pool_data_with_default_config(
      base_protocol_fees: 1248,
      quote0_protocol_fees: 2501278,
      quote1_protocol_fees: 1248,
      inv_native: 12989768785,
      inv: 12984756899,
    )
  let input_value =
    pool_value(
      base_amount: x0,
      quote0_amount: y0,
      quote1_amount: z0,
      lp_amount: 9223372023870018908,
    )
  // Output data:
  let x1 = 1254000
  let y1 = 10744079069
  let z1 = 1000000
  let output_data =
    pool_data_with_default_config(
      base_protocol_fees: 1248,
      quote0_protocol_fees: 2757455,
      quote1_protocol_fees: 1248,
      inv_native: 12984756899,
      inv: 12985012576,
    )
  let output_value =
    pool_value(
      base_amount: x1,
      quote0_amount: y1,
      quote1_amount: z1,
      lp_amount: 9223372023870018908,
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
