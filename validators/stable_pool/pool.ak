use aiken/bytearray
use aiken/int
use aiken/list
use aiken/math
use aiken/string
use aiken/transaction.{
  InlineDatum, Input, Output, OutputReference, ScriptContext, Spend, Transaction,
  TransactionId,
}
use aiken/transaction/credential.{from_script}
use aiken/transaction/value.{Value}
use splash/plutus.{Asset, ScriptHash}
use splash/stableswap/types.{AMMAction, PDAOAction, PoolAction, PoolData}
use splash/stableswap/utils

// LP tokens emisson:
const lp_emission = 340_282_366_920_938_463_463_374_607_431_768_211_455

// Number of non-tradable assets in the pool (pool's NFT + LP token):
const n_non_tradable_tokens = 2

// Common denominator of calculations:
const denom = 100_000

// Number of tokens participating in the swap AMM-operation:
const n_swap_tokens = 2

// Constants for fees numerators averaging in imbalanced deposit/redeem amm-actions:
const imbalance_fee_avg_denom = 4

// Maximum calc error in min token units for swap protocol fees validation:
const max_t_error = 2

/// Main pool validator:
validator(dao_voting_witness: ScriptHash) {
  fn validate_pool(
    input_datum: PoolData,
    action: PoolAction,
    ctx: ScriptContext,
  ) -> Bool {
    let ScriptContext { transaction, purpose } = ctx
    expect Spend(pool_in_utxo_reference) = purpose
    let (pool_in_ix, pool_out_ix) =
      when action is {
        AMMAction(pool_in_ix, pool_out_ix, ..) -> (pool_in_ix, pool_out_ix)

        PDAOAction(pool_in_ix, pool_out_ix) -> (pool_in_ix, pool_out_ix)
      }

    // ==================================== POOL INPUT ==================================== //
    expect Some(pool_input) = list.at(transaction.inputs, pool_in_ix)
    let pool_in_output = pool_input.output
    let input_value = pool_in_output.value

    // Pool input params:
    let pool_in_tokens_policies = value.policies(input_value)
    let pool_in_address = pool_in_output.address

    // ==================================== POOL OUTPUT ==================================== //
    expect Some(pool_out_output) = list.at(transaction.outputs, pool_out_ix)
    // Pool out params:
    let output_value = pool_out_output.value
    let pool_out_tokens_policies = value.policies(output_value)
    let pool_out_address = pool_out_output.address

    // Pool out datum:
    expect output_datum: PoolData = utils.extract_datum(pool_out_output.datum)

    // ==================================== POOL SELF VALIDATIONS ==================================== //
    // 1. Pool input is valid:
    let valid_pool_input: Bool =
      pool_in_utxo_reference == pool_input.output_reference

    // 2. Pool address is preserved:
    let pool_address_preserved: Bool = pool_in_address == pool_out_address
    // 3. Pool NFT is preserved:
    let pool_nft_preserved: Bool =
      value.quantity_of(
        input_value,
        input_datum.pool_nft.policy,
        input_datum.pool_nft.name,
      ) == 1 && value.quantity_of(
        output_value,
        output_datum.pool_nft.policy,
        output_datum.pool_nft.name,
      ) == 1

    // 4. Pool config params are preserved:
    let pool_config_preserved: Bool =
      input_datum.pool_nft == output_datum.pool_nft && input_datum.n == output_datum.n && input_datum.tradable_assets == output_datum.tradable_assets && input_datum.tradable_tokens_multipliers == output_datum.tradable_tokens_multipliers && input_datum.lp_token == output_datum.lp_token && input_datum.ampl_coeff == output_datum.ampl_coeff
    // 5. No extra tokens in the pool's value:
    let allowed_num_tokens = input_datum.n + n_non_tradable_tokens
    let no_more_tokens_in_pool_out: Bool =
      pool_in_tokens_policies == pool_out_tokens_policies && list.length(
        pool_out_tokens_policies,
      ) == allowed_num_tokens

    // 6. Pool out tokens balances are positive:
    let pool_out_tokens_balances =
      utils.extract_target_balances(input_datum.tradable_assets, output_value)
    let positive_pool_out_balances: Bool =
      utils.list_is_positive(pool_out_tokens_balances)
    // 7. Action is valid:
    let valid_action: Bool =
      when action is {
        AMMAction(_, _, option_int0, option_int1) ->
          validate_amm_operation(
            input_value,
            input_datum,
            output_value,
            output_datum,
            option_int0,
            option_int1,
          )

        PDAOAction(..) -> {
          expect Some(dao_stabe_proxy_witness) =
            list.at(input_datum.dao_stabe_proxy_witness, 0)
          let valid_dao_action: Bool =
            utils.contains_withdrawal(
              transaction.withdrawals,
              dao_stabe_proxy_witness,
            )

          let valid_voting: Bool =
            utils.contains_withdrawal(
              transaction.withdrawals,
              dao_voting_witness,
            )

          valid_dao_action && valid_voting
        }
      }
    // Checking whether all validity conditions are met:
    valid_pool_input && pool_address_preserved && pool_nft_preserved && pool_config_preserved && no_more_tokens_in_pool_out && positive_pool_out_balances && valid_action
  }
}

fn validate_amm_operation(
  input_value: Value,
  input_datum: PoolData,
  output_value: Value,
  output_datum: PoolData,
  option_int0: Int,
  option_int1: Int,
) -> Bool {
  // ==================================== INPUT DATA ==================================== //
  // Pool config constants for calculations | Input:
  let n = input_datum.n
  let lp_token = input_datum.lp_token
  let tradable_tokens = input_datum.tradable_assets
  let tradable_tokens_multipliers = input_datum.tradable_tokens_multipliers
  let a = input_datum.ampl_coeff
  let lp_fee_num = input_datum.lp_fee_num
  let protocol_fee_num = input_datum.protocol_fee_num

  // Actual balance of collected protocol fees (accumulated in the pool) | Input:
  let availible_protocol_fees0 = input_datum.protocol_fees

  // ==================================== OUTPUT DATA ==================================== //
  // Actual balance of collected protocol fees (accumulated in the pool) | Output:
  let availible_protocol_fees1 = output_datum.protocol_fees

  // ==================================== CALCULATING DELTAS ==================================== //
  // LP tokens:
  let lp_amount0 =
    value.quantity_of(input_value, lp_token.policy, lp_token.name)
  let lp_amount1 =
    value.quantity_of(output_value, lp_token.policy, lp_token.name)
  let delta_lp = lp_amount1 - lp_amount0
  // Total balances:
  let balances_total0 =
    utils.extract_target_balances(tradable_tokens, input_value)
  let balances_total1 =
    utils.extract_target_balances(tradable_tokens, output_value)

  // Tradable balances:
  // We exclude current protocol fees from the total balances for deposit/swap/redeem actions!
  let tradable_balances0 =
    utils.lists_diff(balances_total0, availible_protocol_fees0)
  let tradable_balances1 =
    utils.lists_diff(balances_total1, availible_protocol_fees1)

  // Collected protocol fees:
  let collected_protocol_fees_deltas =
    utils.lists_diff(availible_protocol_fees1, availible_protocol_fees0)

  // ==================================== AMM-ACTION VALIDATIONS ==================================== //
  // 1. Pool adjustable params are preserved:
  let pool_adjustable_params_preserved: Bool =
    a == output_datum.ampl_coeff && lp_fee_num == output_datum.lp_fee_num && protocol_fee_num == output_datum.protocol_fee_num && input_datum.dao_stabe_proxy_witness == output_datum.dao_stabe_proxy_witness && input_datum.treasury_address == output_datum.treasury_address
  // 2. Checking if AMM-action is valid.
  // Widely used constants:
  let nn = math.pow(n, n)
  let ann = a * nn

  // Invariant values:
  let inv0 = input_datum.inv

  let inv1 = output_datum.inv

  // Converting all balances to the equal precision:
  let tradable_balances_calc1 =
    utils.lists_mult(tradable_balances1, tradable_tokens_multipliers)
  let valid_amm_action =
    if delta_lp != 0 {
      // 2.1. Deposit || Redeem.
      // 2.1.1. Checking the invariant.
      let valid_invariant_extremum: Bool =
        utils.check_invariant_extremum(
          tradable_balances_calc1,
          n,
          nn,
          ann,
          inv1,
        )

      // 2.1.2. Checking the native invariant.
      // Calculating final balances without latest fees:
      let tradable_balances_no_fees1 =
        utils.lists_diff(balances_total1, availible_protocol_fees0)

      let tradable_balances_no_fees_calc1 =
        utils.lists_mult(
          tradable_balances_no_fees1,
          tradable_tokens_multipliers,
        )
      let inv_native = option_int0
      let valid_native_invariant_extremum: Bool =
        utils.check_invariant_extremum(
          tradable_balances_no_fees_calc1,
          n,
          nn,
          ann,
          inv_native,
        )

      // 2.1.3. Checking protocol fees:
      // Calculating ideal balances (uniform deltas case):
      let ideal_tradable_balances1 =
        list.map(tradable_balances0, fn(b) { b * inv_native / inv0 })

      // Calculating difference from the ideal balances:
      let balance_difference_from_ideal =
        utils.lists_abs_diff(
          tradable_balances_no_fees1,
          ideal_tradable_balances1,
        )

      let avg_fee_denom = imbalance_fee_avg_denom * ( n - 1 ) * denom

      let protocol_fees_for_liquidity_operation =
        list.map(
          balance_difference_from_ideal,
          fn(diff) { n * protocol_fee_num * diff / avg_fee_denom },
        )

      let valid_protocol_fees: Bool =
        collected_protocol_fees_deltas == protocol_fees_for_liquidity_operation

      // 2.1.4. Checking liquitidy providers' fees:
      let lp_fees_for_liquidity_operation =
        list.map(
          balance_difference_from_ideal,
          fn(diff) { n * lp_fee_num * diff / avg_fee_denom },
        )
      let tradable_balances_no_lp_fees1 =
        list.map3(
          tradable_balances_no_fees1,
          protocol_fees_for_liquidity_operation,
          lp_fees_for_liquidity_operation,
          fn(a, b, c) { a - b - c },
        )
      let tradable_balances_no_lp_fees_calc1 =
        utils.lists_mult(
          tradable_balances_no_lp_fees1,
          tradable_tokens_multipliers,
        )

      let inv_with_lp_fees = option_int1
      let valid_lp_fees: Bool =
        utils.check_invariant_extremum(
          tradable_balances_no_lp_fees_calc1,
          n,
          nn,
          ann,
          inv_with_lp_fees,
        )

      // 2.1.5. Checking LP token delta:
      let delta_supply_lp =
        ( inv_with_lp_fees - inv0 ) * ( lp_emission - lp_amount0 ) / inv0

      let valid_lp_delta: Bool = -delta_lp == delta_supply_lp
      valid_invariant_extremum && valid_native_invariant_extremum && valid_protocol_fees && valid_lp_fees && valid_lp_delta
    } else {
      // 2.2. Swap.
      // Deltas:
      let tradable_deltas =
        utils.lists_diff(tradable_balances1, tradable_balances0)
      let total_deltas = utils.lists_diff(balances_total1, balances_total0)

      // Base asset info:
      let base_asset_ind = option_int0
      expect Some(tradable_base_asset_delta) =
        list.at(tradable_deltas, base_asset_ind)
      expect Some(base_total_delta) = list.at(total_deltas, base_asset_ind)

      // Quote asset info:
      let quote_asset_ind = option_int1

      expect Some(tradable_quote_asset_delta) =
        list.at(tradable_deltas, quote_asset_ind)
      expect Some(quote_protocol_fees_delta) =
        list.at(collected_protocol_fees_deltas, quote_asset_ind)
      expect Some(quote_total_delta) = list.at(total_deltas, quote_asset_ind)
      expect Some(quote_multiplier) =
        list.at(tradable_tokens_multipliers, quote_asset_ind)

      // 2.2.1. Valid quote protocol fees:
      let total_fees_num_reversed = denom - lp_fee_num - protocol_fee_num
      let valid_quote_protocol_fees: Bool =
        math.abs(
          quote_protocol_fees_delta * total_fees_num_reversed + quote_total_delta * protocol_fee_num,
        ) <= max_t_error * total_fees_num_reversed
      // 2.2.2. Non-quote protocol fees are preserved:
      let non_quote_protocol_fees_preserved: Bool =
        list.count(collected_protocol_fees_deltas, fn(d) { d == 0 }) == n - 1

      // 2.2.3. Valid quote liquidity provider fees:
      let valid_quote_lp_fees: Bool =
        math.abs(
          tradable_quote_asset_delta * total_fees_num_reversed - quote_total_delta * (
            denom - lp_fee_num
          ),
        ) <= max_t_error * total_fees_num_reversed
      // 2.2.4. Non-quote liquidity provider fees are preserved:
      let non_quote_lp_fees_preserved: Bool =
        list.count(tradable_deltas, fn(d) { d == 0 }) == n - n_swap_tokens && tradable_base_asset_delta == base_total_delta
      // 2.2.5. Valid quote tradable delta:
      let valid_quote_tradable_delta: Bool =
        quote_total_delta == tradable_quote_asset_delta + quote_protocol_fees_delta

      // 2.2.5. Valid invariant value:
      let valid_invariant_extremum_for_asset: Bool =
        utils.check_invariant_extremum_for_asset(
          tradable_balances_calc1,
          inv1,
          n,
          nn,
          ann,
          quote_multiplier * n,
          quote_asset_ind,
        )

      // 2.2.6. Invariant value is preserved during calculations.
      // We need to check than valid inv value was used fot swap calculations: 
      let tradable_balances_no_last_lp_fees_calc1 =
        list.indexed_map(
          tradable_balances_calc1,
          fn(i, b) {
            if i == quote_asset_ind {
              (
                b / quote_multiplier * total_fees_num_reversed + quote_total_delta * lp_fee_num
              ) / total_fees_num_reversed * quote_multiplier
            } else {
              b
            }
          },
        )
      let invariant_extremum_for_asset_is_preserved: Bool =
        utils.check_invariant_extremum_for_asset(
          tradable_balances_no_last_lp_fees_calc1,
          inv0,
          n,
          nn,
          ann,
          quote_multiplier * n,
          quote_asset_ind,
        )

      valid_quote_protocol_fees && non_quote_protocol_fees_preserved && valid_quote_lp_fees && non_quote_lp_fees_preserved && valid_quote_tradable_delta && valid_invariant_extremum_for_asset && invariant_extremum_for_asset_is_preserved
    }
  pool_adjustable_params_preserved && valid_amm_action
}

// ================================================= TESTS ================================================= //
const default_lovelace_value = 1_000_000_000_000

// Policies:
pub fn generate_tradable_assets(
  seed: String,
  name_seed: String,
  num: Int,
) -> List<Asset> {
  list.map(
    list.range(0, num - 1),
    fn(x) {
      Asset {
        policy: bytearray.from_string(string.concat(seed, string.from_int(x))),
        name: bytearray.from_string(
          string.concat(name_seed, string.from_int(x)),
        ),
      }
    },
  )
}

test test_generate_tradable_assets() {
  let policies =
    generate_tradable_assets(@"some_pool_token", @"some_pool_token_name", 3)
  list.length(policies) == 3 && list.length(list.unique(policies)) == 3
}

pub fn default_pool_state(
  n: Int,
  tradable_assets: List<Asset>,
  protocol_fees: List<Int>,
  inv: Int,
) -> PoolData {
  let tradable_decimals =
    [1_000, 1_000_000, 1_000]
  let tradable_decimals_sorted = list.sort(tradable_decimals, int.compare)
  expect Some(precision) = list.last(tradable_decimals_sorted)
  let tradable_tokens_multipliers =
    list.map(tradable_decimals, fn(x) { precision / x })
  PoolData {
    pool_nft: Asset {
      policy: bytearray.from_string(@"pool_nft"),
      name: bytearray.from_string(@"pool_nft_name"),
    },
    n,
    tradable_assets,
    tradable_tokens_multipliers,
    lp_token: Asset {
      policy: bytearray.from_string(@"lp_token"),
      name: bytearray.from_string(@"lp_token_name"),
    },
    ampl_coeff: 300,
    lp_fee_num: 200,
    protocol_fee_num: 500,
    dao_stabe_proxy_witness: [bytearray.from_string(@"dao_stabe_proxy_witness")],
    treasury_address: bytearray.from_string(@"treasury_address"),
    protocol_fees,
    inv,
  }
}

/// Returns PoolData and Value with a given params:
pub fn pool_state(
  tradable_balances: List<Int>,
  lp_balance: Int,
  pool_nft_policy_string: String,
  pool_nft_name_string: String,
  n: Int,
  tradable_assets: List<Asset>,
  tradable_decimals: List<Int>,
  lp_policy_string: String,
  lp_name_string: String,
  ampl_coeff: Int,
  lp_fee_num: Int,
  protocol_fee_num: Int,
  dao_policy_string: String,
  treasury_address_string: String,
  protocol_fees: List<Int>,
  inv: Int,
) -> (PoolData, Value) {
  let tradable_decimals_sorted = list.sort(tradable_decimals, int.compare)
  expect Some(precision) = list.last(tradable_decimals_sorted)
  let tradable_tokens_multipliers =
    list.map(tradable_decimals, fn(x) { precision / x })
  let pool_data =
    PoolData {
      pool_nft: Asset {
        policy: bytearray.from_string(pool_nft_policy_string),
        name: bytearray.from_string(pool_nft_name_string),
      },
      n,
      tradable_assets,
      tradable_tokens_multipliers,
      lp_token: Asset {
        policy: bytearray.from_string(lp_policy_string),
        name: bytearray.from_string(lp_name_string),
      },
      ampl_coeff,
      lp_fee_num,
      protocol_fee_num,
      dao_stabe_proxy_witness: [bytearray.from_string(dao_policy_string)],
      treasury_address: bytearray.from_string(treasury_address_string),
      protocol_fees,
      inv,
    }

  expect Some(asset0) = list.at(tradable_assets, 0)
  expect Some(asset1) = list.at(tradable_assets, 1)
  expect Some(balance0) = list.at(tradable_balances, 0)
  expect Some(balance1) = list.at(tradable_balances, 1)

  let min_value =
    value.from_lovelace(default_lovelace_value)
      |> value.add(pool_data.pool_nft.policy, pool_data.pool_nft.name, 1)
      |> value.add(pool_data.lp_token.policy, pool_data.lp_token.name, lp_balance)
      |> value.add(asset0.policy, asset0.name, balance0)
      |> value.add(asset1.policy, asset1.name, balance1)

  let input_value =
    if n == 2 {
      min_value
    } else if n == 3 {
      expect Some(asset2) = list.at(tradable_assets, 2)
      expect Some(balance2) = list.at(tradable_balances, 2)

      let extra_value =
        value.zero()
          |> value.add(asset2.policy, asset2.name, balance2)
      value.merge(min_value, extra_value)
    } else if n == 4 {
      expect Some(asset2) = list.at(tradable_assets, 2)
      expect Some(balance2) = list.at(tradable_balances, 2)
      expect Some(asset3) = list.at(tradable_assets, 3)
      expect Some(balance3) = list.at(tradable_balances, 3)

      let extra_value =
        value.zero()
          |> value.add(asset2.policy, asset2.name, balance2)
          |> value.add(asset3.policy, asset3.name, balance3)
      value.merge(min_value, extra_value)
    } else {
      value.zero()
    }

  (pool_data, input_value)
}

pub fn pool_amm_action_state_transition_tx(
  input_data: PoolData,
  input_value: Value,
  output_data: PoolData,
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
  let min_value = 1_000_000
  let default_decimal = 1_000_000
  let default_token_value = min_value * default_decimal

  let tradable_assets =
    generate_tradable_assets(@"some_pool_token", @"some_pool_token_name", n)

  // Input data:
  let total_balances_before = list.repeat(default_token_value, n)
  let tradable_decimals_before = list.repeat(default_decimal, n)
  let collected_protocol_fees_before = list.repeat(0, n)
  let a_before = 1000

  let inv_before = default_token_value * n
  let lp_balance_before = lp_emission - inv_before

  let (input_pool_data, input_value) =
    pool_state(
      total_balances_before,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      a_before,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
      inv_before,
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
          b - base_amount + quote_lp_fees + quote_protocol_fees
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

  let inv_after = inv_before + quote_lp_fees

  let (output_pool_data, output_value) =
    pool_state(
      total_balances_after,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      a_before,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after,
      inv_after,
    )
  // Validation:
  validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data,
    base_ind,
    quote_ind,
  )
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
    generate_tradable_assets(@"some_pool_token", @"some_pool_token_name", n)

  // Input data:
  let total_balances_before = list.repeat(default_token_value, n)
  let tradable_decimals_before = list.repeat(default_decimal, n)
  let collected_protocol_fees_before = list.repeat(0, n)
  let a_before = 1000

  let inv_before = default_token_value * n
  let lp_balance_before = lp_emission - inv_before

  let (input_pool_data, input_value) =
    pool_state(
      total_balances_before,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      a_before,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
      inv_before,
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

  let inv_after = inv_before + quote_lp_fees + quote_protocol_fees
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
    pool_state(
      total_balances_after,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      a_before,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after,
      inv_after,
    )
  let (_, output_value_wrong_balance) =
    pool_state(
      total_balances_after_wrong,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      a_before,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after,
      inv_after,
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
    pool_state(
      total_balances_after,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      a_before,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after_wrong,
      inv_after,
    )
  // Wrong invariants:
  let inv_after_wrong = inv_before + quote_lp_fees - quote_protocol_fees - 1

  let (output_pool_data_wrong_inv, _) =
    pool_state(
      total_balances_after,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      a_before,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after,
      inv_after_wrong,
    )

  let (output_pool_data_wrong_inv_and_fees, _) =
    pool_state(
      total_balances_after,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      a_before,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after_wrong,
      inv_after_wrong,
    )
  // Validations:
  validate_amm_operation(
    input_value,
    input_pool_data,
    output_value_wrong_balance,
    output_pool_data,
    base_ind,
    quote_ind,
  ) && validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_protocol_fees,
    base_ind,
    quote_ind,
  ) && validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_inv,
    base_ind,
    quote_ind,
  ) && validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_inv_and_fees,
    base_ind,
    quote_ind,
  )
}

// // =================================== DEPOSIT/REDEEM TEST TEMPLATES =================================== //
fn uniform_reserves_valid_lq_math_template(
  n: Int,
  delta: Int,
  a_before: Int,
  lp_fee_num_before: Int,
  protocol_fee_num_before: Int,
  deposit: Bool,
) -> Bool {
  let default_token_value = 1_000_000_000_000_000
  let default_decimal = 10

  let tradable_assets =
    generate_tradable_assets(@"some_pool_token", @"some_pool_token_name", n)

  // Input data:
  let total_balances_before = list.repeat(default_token_value, n)
  let tradable_decimals_before = list.repeat(default_decimal, n)
  let collected_protocol_fees_before = list.repeat(0, n)

  let inv_before = default_token_value * n
  let lp_balance_before = lp_emission - inv_before

  let (input_pool_data, input_value) =
    pool_state(
      total_balances_before,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      a_before,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
      inv_before,
    )
  // Output data:
  let new_token_value =
    if deposit {
      default_token_value + delta
    } else {
      default_token_value - delta
    }

  let total_balances_after = list.repeat(new_token_value, n)

  let inv_after = new_token_value * n
  let lp_balance_after = lp_emission - inv_after
  let (output_pool_data, output_value) =
    pool_state(
      total_balances_after,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      a_before,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
      inv_after,
    )
  // Validation:
  validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data,
    inv_after,
    inv_after,
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
  let default_decimal = 1

  let tradable_assets =
    generate_tradable_assets(@"some_pool_token", @"some_pool_token_name", n)

  // Input data:
  let total_balances_before = list.repeat(default_token_value, n)
  let tradable_decimals_before = list.repeat(default_decimal, n)
  let collected_protocol_fees_before = list.repeat(default_token_value, n)

  let inv_before = default_token_value * n
  let lp_balance_before = lp_emission - inv_before

  let (input_pool_data, input_value) =
    pool_state(
      total_balances_before,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      a_before,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
      inv_before,
    )

  // Output data:
  let new_token_value =
    if deposit {
      default_token_value + delta
    } else {
      default_token_value - delta
    }

  let new_token_value_wrong = new_token_value - 1
  let total_balances_after = list.repeat(new_token_value, n)
  let total_balances_after_wrong = list.repeat(new_token_value_wrong, n)

  let inv_after = new_token_value * n
  let inv_after_wrong = new_token_value * n - 1
  let lp_balance_after = lp_emission - inv_after
  let lp_balance_after_wrong = lp_emission - inv_after - 1

  let collected_protocol_fees_after_wrong =
    list.repeat(default_token_value - 1, n)

  let (output_pool_data, output_value_wrong_balance) =
    pool_state(
      total_balances_after_wrong,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      a_before,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
      inv_after,
    )
  let (output_pool_data_wrong_protocol_fees, output_value) =
    pool_state(
      total_balances_after,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      a_before,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after_wrong,
      inv_after,
    )
  let (output_pool_data_wrong_inv, _) =
    pool_state(
      total_balances_after,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      a_before,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
      inv_after_wrong,
    )

  let (_, output_value_wrong_lp) =
    pool_state(
      total_balances_after,
      lp_balance_after_wrong,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      a_before,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
      inv_after,
    )
  // Validations:
  validate_amm_operation(
    input_value,
    input_pool_data,
    output_value_wrong_balance,
    output_pool_data,
    inv_after,
    inv_after,
  ) && validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_protocol_fees,
    inv_after,
    inv_after,
  ) && validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_inv,
    inv_after,
    inv_after,
  ) && validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_inv,
    inv_after,
    inv_after,
  ) && validate_amm_operation(
    input_value,
    input_pool_data,
    output_value_wrong_lp,
    output_pool_data,
    inv_after,
    inv_after,
  )
}

// =================================== DEPOSIT TESTS =================================== //
test test_uniform_deposit_with_valid_math() {
  let lp_fee_num = 112
  let protocol_fee_num = 300
  let n_list =
    [2, 3, 4]
  let deposited_amount_list =
    [1_000_000, 3_000_000_004, 950_040_703_214]
  let a_list =
    [1, 350, 11000]
  let validation_results0 =
    list.map3(
      n_list,
      deposited_amount_list,
      a_list,
      fn(n, delta, a) {
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
  let n_cases = list.length(n_list)
  list.count(validation_results0, fn(r) { r == True }) == n_cases
}

test test_uniform_deposit_with_invalid_math() {
  let lp_fee_num = 112
  let protocol_fee_num = 300
  let n_list =
    [2, 3, 4]
  let deposited_amount_list =
    [1_000_000, 3_000_000_004, 950_040_703_214]
  let a_list =
    [1, 350, 11000]
  let validation_results0 =
    list.map3(
      n_list,
      deposited_amount_list,
      a_list,
      fn(n, delta, a) {
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
  let n_cases = list.length(n_list)
  list.count(validation_results0, fn(r) { r == False }) == n_cases
}

test test_random_deposit() {
  let n = 3
  let a = 3000
  let lp_fee_num = 200
  let protocol_fee_num = 500
  let tradable_assets =
    generate_tradable_assets(@"some_pool_token", @"some_pool_token_name", n)
  let tradable_decimals =
    [1_000, 1_000_000, 1_000]
  // Input data:
  let total_balances_before =
    [10476100039226, 8543419899, 97149583]
  let collected_protocol_fees_before =
    [3922600039226, 1989919899, 31614583]
  let inv_before = 440436474588133
  let lp_balance_before = lp_emission - inv_before

  let (input_pool_data, input_value) =
    pool_state(
      total_balances_before,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      a,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
      inv_before,
    )
  // Output data:
  let total_balances_after =
    [12059477004396, 607565348976096, 3331844339354]
  let total_balances_after_wrong =
    [12059477004393, 607565348976095, 333184433935]
  let collected_protocol_fees_after =
    [4244248533159, 1140834312991, 6275394390]
  let collected_protocol_fees_after_wrong =
    [4574056297365, 1282900260424, 7052970847]
  let inv_after = 11746814561387838
  let inv_native = 12075811814236382
  let inv_with_lp_fees = 11615215184277714

  let lp_balance_after = lp_emission - inv_with_lp_fees

  let (output_pool_data, output_value) =
    pool_state(
      total_balances_after,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      a,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after,
      inv_after,
    )
  // Output with error:
  // 1. Wrong balances:
  let (_, output_value_wrong_balances) =
    pool_state(
      total_balances_after_wrong,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      a,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after,
      inv_after,
    )
  // 2. Wrong fees:
  let (output_pool_data_wrong_fees, _) =
    pool_state(
      total_balances_after,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      a,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after_wrong,
      inv_after,
    )

  // 3. Wrong invariant:
  let inv_after_wrong = inv_after - 1
  let (output_pool_data_wrong_inv, _) =
    pool_state(
      total_balances_after,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      a,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after_wrong,
      inv_after_wrong,
    )
  // Validations:
  validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data,
    inv_native,
    inv_with_lp_fees,
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value_wrong_balances,
    output_pool_data,
    inv_native,
    inv_with_lp_fees,
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_fees,
    inv_native,
    inv_with_lp_fees,
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_fees,
    inv_native,
    inv_with_lp_fees,
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_inv,
    inv_native,
    inv_with_lp_fees,
  )
}

// // =================================== REDEEM TESTS =================================== //
test test_uniform_redeem_with_valid_math() {
  let lp_fee_num = 112
  let protocol_fee_num = 300
  let n_list =
    [2, 3, 4]
  let deposited_amount_list =
    [1_000_000, 3_000_000_004, 950_040_703_214]
  let a_list =
    [1, 350, 11000]
  let validation_results0 =
    list.map3(
      n_list,
      deposited_amount_list,
      a_list,
      fn(n, delta, a) {
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
  let n_cases = list.length(n_list)
  list.count(validation_results0, fn(r) { r == True }) == n_cases
}

test test_uniform_redeem_with_invalid_math() {
  let lp_fee_num = 112
  let protocol_fee_num = 300
  let n_list =
    [2, 3, 4]
  let deposited_amount_list =
    [1_000_000, 3_000_000_004, 950_040_703_214]
  let a_list =
    [1, 350, 11000]
  let validation_results0 =
    list.map3(
      n_list,
      deposited_amount_list,
      a_list,
      fn(n, delta, a) {
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
  let n_cases = list.length(n_list)
  list.count(validation_results0, fn(r) { r == False }) == n_cases
}

test test_random_redeem() {
  let n = 3
  let a = 300
  let lp_fee_num = 200
  let protocol_fee_num = 500
  let tradable_assets =
    generate_tradable_assets(@"some_pool_token", @"some_pool_token_name", n)
  let tradable_decimals =
    [1_000, 1_000_000, 1_000]
  // Input data:
  let total_balances_before =
    [8902100023486, 10811342578, 75424880]
  let collected_protocol_fees_before =
    [2348600023486, 4257842578, 9889880]
  let inv_before = 249586736899936
  let lp_balance_before = lp_emission - inv_before

  let (input_pool_data, input_value) =
    pool_state(
      total_balances_before,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      a,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
      inv_before,
    )
  // Output data:
  let total_balances_after =
    [8902099890353, 10661342428, 75359815]
  let collected_protocol_fees_after =
    [2348673260795, 4258050590, 9890490]
  let inv_after = 248095188067828
  let inv_native = 248099155340753
  let inv_with_lp_fees = 248093601150476

  let total_balances_after_wrong =
    [8902099890351, 10661342428, 75359813]
  let collected_protocol_fees_after_wrong =
    [2348673260795, 4258050590, 9890304]
  let lp_balance_after = lp_emission - inv_with_lp_fees

  let (output_pool_data, output_value) =
    pool_state(
      total_balances_after,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      a,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after,
      inv_after,
    )
  // Output with error:
  // 1. Wrong balances:
  let (_, output_value_wrong_balances) =
    pool_state(
      total_balances_after_wrong,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      a,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after,
      inv_after,
    )
  // 2. Wrong fees:
  let (output_pool_data_wrong_fees, _) =
    pool_state(
      total_balances_after,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      a,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after_wrong,
      inv_after,
    )

  // 3. Wrong invariant:
  let inv_after_wrong = inv_after - 1
  let (output_pool_data_wrong_inv, _) =
    pool_state(
      total_balances_after,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      a,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after_wrong,
      inv_after_wrong,
    )
  // Validations:
  validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data,
    inv_native,
    inv_with_lp_fees,
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value_wrong_balances,
    output_pool_data,
    inv_native,
    inv_with_lp_fees,
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_fees,
    inv_native,
    inv_with_lp_fees,
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_fees,
    inv_native,
    inv_with_lp_fees,
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_inv,
    inv_native,
    inv_with_lp_fees,
  )
}

// =================================== SWAP TESTS =================================== //
test test_uniform_swap_with_valid_math() {
  // Swap 0 -> 1
  let base_ind0 = 0
  let quote_ind0 = 1
  let protocol_fee_num = 2000
  let n_list =
    [2, 3, 4]
  let base_amount_list =
    [1_000_000, 30_000, 950_000]
  let swap_fee_list =
    [100, 250, 1000]
  let validation_results0 =
    list.map3(
      n_list,
      base_amount_list,
      swap_fee_list,
      fn(n, base, swap_fee) {
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
      n_list,
      base_amount_list,
      swap_fee_list,
      fn(n, base, swap_fee) {
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
  // Protocol fee variation:
  let lp_fee_num = 110
  let protocol_fee_list =
    [500, 2150, 5000]
  let validation_results2 =
    list.map3(
      n_list,
      base_amount_list,
      protocol_fee_list,
      fn(n, base, p_fee) {
        uniform_reserves_valid_swap_math_template(
          n,
          base,
          lp_fee_num,
          p_fee,
          base_ind1,
          quote_ind1,
        )
      },
    )
  let n_cases = list.length(n_list)
  list.count(validation_results0, fn(r) { r == True }) == n_cases && list.count(
    validation_results1,
    fn(r) { r == True },
  ) == n_cases && list.count(validation_results2, fn(r) { r == True }) == n_cases
}

test test_uniform_swap_with_invalid_math() {
  // Swap 0 -> 1
  let base_ind0 = 0
  let quote_ind0 = 1
  let protocol_fee_num = 300
  let n_list =
    [2, 3, 4]
  let base_amount_list =
    [1_000_000, 30_000, 950_000]
  let swap_fee_list =
    [100, 200, 1100]
  let validation_results0 =
    list.map3(
      n_list,
      base_amount_list,
      swap_fee_list,
      fn(n, base, swap_fee) {
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
      n_list,
      base_amount_list,
      swap_fee_list,
      fn(n, base, swap_fee) {
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
  // Protocol fee variation:
  let lp_fee_num = 110
  let protocol_fee_list =
    [500, 2000, 5100]
  let validation_results2 =
    list.map3(
      n_list,
      base_amount_list,
      protocol_fee_list,
      fn(n, base, p_fee) {
        uniform_reserves_invalid_swap_math_template(
          n,
          base,
          lp_fee_num,
          p_fee,
          base_ind1,
          quote_ind1,
        )
      },
    )

  let n_cases = list.length(n_list)
  list.count(validation_results0, fn(r) { r == False }) == n_cases && list.count(
    validation_results1,
    fn(r) { r == False },
  ) == n_cases && list.count(validation_results2, fn(r) { r == False }) == n_cases
}

test test_random_swap_to_max_decimals() {
  // 0 -> 1
  let base_ind = 0
  let quote_ind = 1
  let n = 3
  let a = 300
  let lp_fee_num = 20000
  let protocol_fee_num = 50000
  let tradable_assets =
    generate_tradable_assets(@"some_pool_token", @"some_pool_token_name", n)
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
    pool_state(
      total_balances_before,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      a,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
      inv_before,
    )
  // Output data:
  let total_balances_after =
    [390088, 460901216, 348093]

  let collected_protocol_fees_after =
    [88088, 243498560, 93093]

  let inv_after = 774400041
  let lp_balance_after = lp_balance_before
  let (output_pool_data, output_value) =
    pool_state(
      total_balances_after,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      a,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after,
      inv_after,
    )
  // Output with error:
  // 1. Wrong balances:
  let total_balances_after_wrong =
    [390088, 428943460, 348092]
  let (_, output_value_wrong_balances) =
    pool_state(
      total_balances_after_wrong,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      a,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after,
      inv_after,
    )
  // 2. Wrong fees:
  let collected_protocol_fees_after_wrong =
    [88088, 220047215, 93093]
  let (output_pool_data_wrong_fees, _) =
    pool_state(
      total_balances_after,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      a,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after_wrong,
      inv_after,
    )

  // 3. Wrong invariant:
  let inv_after_wrong = inv_after - 1
  let (output_pool_data_wrong_inv, _) =
    pool_state(
      total_balances_after,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      a,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after_wrong,
      inv_after_wrong,
    )
  // Validations:
  validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data,
    base_ind,
    quote_ind,
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value_wrong_balances,
    output_pool_data,
    base_ind,
    quote_ind,
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_fees,
    base_ind,
    quote_ind,
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_fees,
    base_ind,
    quote_ind,
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_inv,
    base_ind,
    quote_ind,
  )
}

test test_random_swap_to_min_decimals() {
  // 0 -> 1
  let base_ind = 0
  let quote_ind = 1
  let n = 3
  let a = 300
  let lp_fee_num = 20000
  let protocol_fee_num = 50000
  let tradable_assets =
    generate_tradable_assets(@"some_pool_token", @"some_pool_token_name", n)
  let tradable_decimals =
    [1_000_000, 1_000, 1_000_000]
  // Input data:
  let total_balances_before =
    [343088, 475000220, 348093]
  let collected_protocol_fees_before =
    [88088, 220000220, 93093]
  let inv_before = 5484565172
  let lp_balance_before = lp_emission - inv_before

  let (input_pool_data, input_value) =
    pool_state(
      total_balances_before,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      a,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
      inv_before,
    )
  // Output data:
  let total_balances_after =
    [390088, 468863263, 348093]

  let collected_protocol_fees_after =
    [88088, 230228480, 93093]

  let inv_after = 5532477203
  let lp_balance_after = lp_balance_before
  let (output_pool_data, output_value) =
    pool_state(
      total_balances_after,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      a,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after,
      inv_after,
    )
  // Output with error:
  // 1. Wrong balances:
  let total_balances_after_wrong =
    [390088, 428943460, 348092]
  let (_, output_value_wrong_balances) =
    pool_state(
      total_balances_after_wrong,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      a,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after,
      inv_after,
    )
  // 2. Wrong fees:
  let collected_protocol_fees_after_wrong =
    [88088, 220047215, 93093]
  let (output_pool_data_wrong_fees, _) =
    pool_state(
      total_balances_after,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      a,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after_wrong,
      inv_after,
    )

  // 3. Wrong invariant:
  let inv_after_wrong = inv_after - 1
  let (output_pool_data_wrong_inv, _) =
    pool_state(
      total_balances_after,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
      n,
      tradable_assets,
      tradable_decimals,
      @"pool_lp_policy",
      @"pool_lp_name",
      a,
      lp_fee_num,
      protocol_fee_num,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after_wrong,
      inv_after_wrong,
    )
  // Validations:
  validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data,
    base_ind,
    quote_ind,
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value_wrong_balances,
    output_pool_data,
    base_ind,
    quote_ind,
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_fees,
    base_ind,
    quote_ind,
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_fees,
    base_ind,
    quote_ind,
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_inv,
    base_ind,
    quote_ind,
  )
}
