use aiken/bytearray
use aiken/int
use aiken/list
use aiken/math
use aiken/transaction.{
  InlineDatum, Input, Output, ScriptContext, Spend, Transaction,
}
use aiken/transaction/credential.{Address}
use aiken/transaction/value.{Value}
use splash/plutus.{Asset, ScriptHash}
use splash/stableswap/test_utils.{
  generate_tradable_assets, make_pool_state_transition_context, n2t_pool_state,
  t2t_pool_state,
}
use splash/stableswap/types.{AMMAction, PDAOAction, PoolData, PoolRedeemer}
use splash/stableswap/utils

// LP tokens emisson:
const lp_emission = 9223372036854775807

// Common denominator of calculations:
const denom = 100_000

// Number of tokens participating in the swap AMM-operation:
const n_tradable_tokens = 2

/// Main pool validator:
validator(dao_voting_witness: ScriptHash) {
  fn validate_stable_pool_transition(
    input_datum: PoolData,
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
      address: Address { payment_credential: payment_cred_in, .. },
      ..
    } = pool_input_output

    let PoolData {
      pool_nft: pool_nft_in,
      n: n_in,
      tradable_assets: tradable_assets_in,
      tradable_tokens_multipliers: tradable_tokens_multipliers_in,
      lp_token: lp_token_in,
      ampl_coeff_is_editable: ampl_coeff_is_editable_in,
      lp_fee_is_editable: lp_fee_is_editable_in,
      dao_stabe_proxy_witness,
      ..
    } = input_datum

    let pool_in_tokens_policies = value.policies(input_value)

    // ==================================== POOL OUTPUT ==================================== //
    expect Some(pool_out_output) = list.at(outputs, pool_out_ix)
    // Pool out params:
    expect Output {
      value: output_value,
      address: Address { payment_credential: payment_cred_out, .. },
      datum: InlineDatum(pool_out_datum),
      ..
    } = pool_out_output
    expect output_datum: PoolData = pool_out_datum
    let pool_out_tokens_policies = value.policies(output_value)

    let PoolData {
      pool_nft: pool_nft_out,
      n: n_out,
      tradable_assets: tradable_assets_out,
      tradable_tokens_multipliers: tradable_tokens_multipliers_out,
      lp_token: lp_token_out,
      ampl_coeff_is_editable: ampl_coeff_is_editable_out,
      lp_fee_is_editable: lp_fee_is_editable_out,
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
      pool_nft_in == pool_nft_out && n_in == n_out && tradable_assets_in == tradable_assets_out && tradable_tokens_multipliers_in == tradable_tokens_multipliers_out && lp_token_in == lp_token_out && lp_fee_is_editable_in == lp_fee_is_editable_out && ampl_coeff_is_editable_in == ampl_coeff_is_editable_out
    // 5. No extra tokens in the pool's value:
    let no_more_tokens_in_pool_out: Bool =
      pool_in_tokens_policies == pool_out_tokens_policies
    // 6. Action is valid:
    let valid_action: Bool =
      when action is {
        AMMAction(context_values_list) ->
          validate_amm_operation(
            input_value,
            input_datum,
            output_value,
            output_datum,
            context_values_list,
          )

        PDAOAction -> {
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
    valid_pool_input && pool_script_preserved && pool_nft_preserved && pool_config_preserved && no_more_tokens_in_pool_out && valid_action
  }
}

fn validate_amm_operation(
  input_value: Value,
  input_datum: PoolData,
  output_value: Value,
  output_datum: PoolData,
  context_values_list: List<Int>,
) -> Bool {
  // ==================================== INPUT DATA ==================================== //
  // Pool config constants for calculations | Input:
  let PoolData {
    tradable_assets,
    tradable_tokens_multipliers,
    lp_token,
    n,
    an2n,
    lp_fee_num,
    protocol_fee_num,
    treasury_address,
    protocol_fees: availible_protocol_fees0,
    alpha: alpha0,
    betta: betta0,
    ..
  } = input_datum

  let Asset { policy: lp_token_policy, name: lp_token_name } = lp_token

  // Pool data | Output:
  let PoolData {
    an2n: an2n1,
    lp_fee_num: lp_fee_num1,
    protocol_fee_num: protocol_fee_num1,
    treasury_address: treasury_address1,
    protocol_fees: availible_protocol_fees1,
    alpha: alpha1,
    betta: betta1,
    ..
  } = output_datum
  // ==================================== CALCULATING DELTAS ==================================== //
  // LP tokens:
  let lp_amount0 =
    value.quantity_of(input_value, lp_token_policy, lp_token_name)
  let lp_amount1 =
    value.quantity_of(output_value, lp_token_policy, lp_token_name)
  let delta_lp = lp_amount1 - lp_amount0
  // Collected protocol fees:
  let collected_protocol_fees_deltas =
    utils.lists_diff(availible_protocol_fees1, availible_protocol_fees0)

  // =============================== CALCULATING COMMONLY USED VALUES =============================== //
  // Total balances:
  let balances_total0 =
    utils.extract_target_balances(tradable_assets, input_value)
  let balances_total1 =
    utils.extract_target_balances(tradable_assets, output_value)

  // Tradable balances:
  // We exclude current protocol fees from the total balances for deposit/swap/redeem actions!
  let tradable_balances0 =
    utils.lists_diff(balances_total0, availible_protocol_fees0)
  let tradable_balances1 =
    utils.lists_diff(balances_total1, availible_protocol_fees1)
  // Convert to common precision for calculations:
  let tradable_balances_calc1 =
    utils.lists_mult(tradable_balances1, tradable_tokens_multipliers)

  // Widely used constanst:
  let total_sum_calc_after = utils.sum(tradable_balances_calc1)
  let total_prod_calc_after = utils.prod(tradable_balances_calc1)
  let nn = math.pow(n, n)
  expect Some(d1) = list.at(context_values_list, 0)
  let d1n1 = math.pow(d1, n + 1)

  // ==================================== AMM-ACTION VALIDATIONS ==================================== //
  // 1. Pool adjustable params are preserved:
  let pool_adjustable_params_preserved: Bool =
    an2n == an2n1 && lp_fee_num == lp_fee_num1 && protocol_fee_num == protocol_fee_num1 && treasury_address == treasury_address1

  // 2. Checking if constants alpha and betta are valid in the output datum:
  let alpha_after = ( an2n - nn ) * total_prod_calc_after
  let betta_after = an2n * total_prod_calc_after * total_sum_calc_after
  let valid_alpha_betta: Bool = alpha1 == alpha_after && betta1 == betta_after

  // 3. Checking if AMM-action is valid.
  let valid_amm_action =
    if delta_lp != 0 {
      // 3.1. Deposit || Redeem.
      // 3.1.1. Checking the invariant.
      let valid_invariant_for_lq_operation: Bool =
        utils.check_invariant(
          nn,
          an2n,
          alpha0,
          betta0,
          d1,
          d1n1,
          total_sum_calc_after,
          total_prod_calc_after,
        )

      // 3.1.2. Checking LP token delta:
      let supply_lp = lp_emission - lp_amount0
      let delta_supply_lp_candidates =
        list.map2(
          tradable_balances0,
          tradable_balances1,
          fn(b0, b1) { supply_lp * b1 / b0 - supply_lp },
        )
      let delta_supply_lp_candidates_sorted =
        list.sort(delta_supply_lp_candidates, int.compare)
      expect Some(delta_supply_lp) =
        list.head(delta_supply_lp_candidates_sorted)
      let valid_lp_delta: Bool = -delta_lp == delta_supply_lp

      valid_invariant_for_lq_operation && valid_lp_delta
    } else {
      // 3.2. Swap.
      // Deltas:
      let tradable_deltas =
        utils.lists_diff(tradable_balances1, tradable_balances0)
      let total_deltas = utils.lists_diff(balances_total1, balances_total0)

      // Base asset info:
      expect Some(base_asset_ind) = list.at(context_values_list, 1)
      expect Some(tradable_base_asset_delta) =
        list.at(tradable_deltas, base_asset_ind)
      expect Some(base_total_delta) = list.at(total_deltas, base_asset_ind)

      // Quote asset info:
      expect Some(quote_asset_ind) = list.at(context_values_list, 2)

      expect Some(tradable_quote_asset_delta) =
        list.at(tradable_deltas, quote_asset_ind)

      expect Some(quote_protocol_fees_delta) =
        list.at(collected_protocol_fees_deltas, quote_asset_ind)

      expect Some(quote_total_delta) = list.at(total_deltas, quote_asset_ind)

      // 3.2.1. Valid quote protocol fees:
      let total_fees_num_reversed = denom - lp_fee_num - protocol_fee_num
      let valid_quote_protocol_fees: Bool =
        quote_protocol_fees_delta * total_fees_num_reversed >= -quote_total_delta * protocol_fee_num
      // 3.2.2. Non-quote protocol fees are preserved:
      let non_quote_protocol_fees_preserved: Bool =
        list.count(collected_protocol_fees_deltas, fn(d) { d == 0 }) == n - 1

      // 3.2.3. Valid quote liquidity provider fees:
      let valid_quote_lp_fees: Bool =
        tradable_quote_asset_delta * total_fees_num_reversed >= quote_total_delta * (
          denom - lp_fee_num
        )
      // 3.2.4. Non-quote liquidity provider fees are preserved:
      let non_quote_lp_fees_preserved: Bool =
        list.count(tradable_deltas, fn(d) { d == 0 }) == n - n_tradable_tokens && tradable_base_asset_delta == base_total_delta
      // 3.2.5. Valid quote tradable delta:
      let valid_quote_tradable_delta: Bool =
        quote_total_delta == tradable_quote_asset_delta + quote_protocol_fees_delta

      // 3.2.6. Valid invariant value:
      let valid_invariant_for_swap: Bool =
        utils.check_invariant(
          nn,
          an2n,
          alpha0,
          betta0,
          d1,
          d1n1,
          total_sum_calc_after,
          total_prod_calc_after,
        )

      valid_quote_protocol_fees && non_quote_protocol_fees_preserved && valid_quote_lp_fees && non_quote_lp_fees_preserved && valid_quote_tradable_delta && valid_invariant_for_swap
    }
  pool_adjustable_params_preserved && valid_alpha_betta && valid_amm_action
}

// ================================================= TESTS ================================================= //

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
    t2t_pool_state(
      n,
      total_balances_before,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
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
    )
  let tradable_assets_n2t = list.drop(tradable_assets, 1)
  let tradable_assets_n2t =
    list.push(tradable_assets_n2t, Asset { policy: #"", name: #"" })

  let (input_pool_data_n2t, input_value_n2t) =
    n2t_pool_state(
      n,
      total_balances_before,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets_n2t,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      a_before,
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

  let inv_after = inv_before + quote_lp_fees - 1

  let (output_pool_data, output_value) =
    t2t_pool_state(
      n,
      total_balances_after,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
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
    )

  let (output_pool_data_n2t, output_value_nt2) =
    n2t_pool_state(
      n,
      total_balances_after,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
      tradable_assets_n2t,
      tradable_decimals_before,
      @"pool_lp_policy",
      @"pool_lp_name",
      a_before,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_after,
    )
  let ctx =
    make_pool_state_transition_context(
      input_pool_data,
      input_value,
      output_pool_data,
      output_value,
    )
  let redeemer = PoolRedeemer(0, 0, AMMAction([inv_after, base_ind, quote_ind]))
  // Validation:
  validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data,
    [inv_after, base_ind, quote_ind],
  ) && validate_amm_operation(
    input_value_n2t,
    input_pool_data_n2t,
    output_value_nt2,
    output_pool_data_n2t,
    [inv_after, base_ind, quote_ind],
  ) && validate_stable_pool_transition(
    bytearray.from_string(@"proxy_dao"),
    input_pool_data,
    redeemer,
    ctx,
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
    t2t_pool_state(
      n,
      total_balances_before,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
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
    t2t_pool_state(
      n,
      total_balances_after,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
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
    )
  let (_, output_value_wrong_balance) =
    t2t_pool_state(
      n,
      total_balances_after_wrong,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
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
    t2t_pool_state(
      n,
      total_balances_after,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
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
    )
  // Wrong invariants:
  let inv_after_wrong = inv_before + quote_lp_fees - quote_protocol_fees - 1

  // Validations:
  validate_amm_operation(
    input_value,
    input_pool_data,
    output_value_wrong_balance,
    output_pool_data,
    [inv_after, base_ind, quote_ind],
  ) && validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_protocol_fees,
    [inv_after, base_ind, quote_ind],
  ) && validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data,
    [inv_after_wrong, base_ind, quote_ind],
  ) && validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_protocol_fees,
    [inv_after_wrong, base_ind, quote_ind],
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
  let default_decimal = 10

  let tradable_assets =
    generate_tradable_assets(@"some_pool_token", @"some_pool_token_name", n)

  // Input data:
  let total_balances_before = list.repeat(default_token_value, n)
  let tradable_decimals_before = list.repeat(default_decimal, n)
  let collected_protocol_fees_before = list.repeat(0, n)

  let inv_before = default_token_value * n
  let lp_balance_before = lp_emission - inv_before
  let an2n = a_before * math.pow(n, 2 * n)
  let (input_pool_data, input_value) =
    t2t_pool_state(
      n,
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

  let inv_after =
    if deposit {
      new_token_value * n
    } else {
      ( new_token_value + delta / 2 ) * n
    }
  let lp_balance_after = lp_emission - new_token_value * n
  let (output_pool_data, output_value) =
    t2t_pool_state(
      n,
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
  // Validation:
  validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data,
    [inv_after],
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
    generate_tradable_assets(@"some_pool_token", @"some_pool_token_name", n)

  // Input data:
  let total_balances_before = list.repeat(default_token_value, n)
  let tradable_decimals_before = list.repeat(default_decimal, n)
  let collected_protocol_fees_before =
    list.repeat(default_protocol_fee_value, n)

  let inv_before = default_token_value * n
  let lp_balance_before = lp_emission - inv_before

  let (input_pool_data, input_value) =
    t2t_pool_state(
      n,
      total_balances_before,
      lp_balance_before,
      @"pool_nft_policy",
      @"pool_nft_name",
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

  let inv_after = new_token_value * n
  let inv_after_wrong = new_token_value * n - 1
  let lp_balance_after = lp_emission - inv_after
  let lp_balance_after_wrong = lp_emission - inv_after - 1

  // Wrong balances:
  let collected_protocol_fees_after_wrong =
    list.repeat(default_token_value - 1, n)

  let (output_pool_data, output_value_wrong_balance) =
    t2t_pool_state(
      n,
      total_balances_after_wrong,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
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
    )

  // Wrong protocol fees:
  let (output_pool_data_wrong_protocol_fees, output_value) =
    t2t_pool_state(
      n,
      total_balances_after,
      lp_balance_after,
      @"pool_nft_policy",
      @"pool_nft_name",
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
    )

  // Wrong LP balance:
  let (_, output_value_wrong_lp) =
    t2t_pool_state(
      n,
      total_balances_after,
      lp_balance_after_wrong,
      @"pool_nft_policy",
      @"pool_nft_name",
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
    )
  // Validations:
  validate_amm_operation(
    input_value,
    input_pool_data,
    output_value_wrong_balance,
    output_pool_data,
    [inv_after],
  ) && validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_protocol_fees,
    [inv_after],
  ) && validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data,
    [inv_after_wrong],
  ) && validate_amm_operation(
    input_value,
    input_pool_data,
    output_value_wrong_lp,
    output_pool_data,
    [inv_after],
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

test test_swap_to_max_decimals_fixtures() {
  // 0 -> 1
  let base_ind = 0
  let quote_ind = 1
  let n = 3
  let a = 300
  let an2n = a * math.pow(n, 2 * n)
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
    t2t_pool_state(
      n,
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
    [390088, 460901216, 348093]

  let collected_protocol_fees_after =
    [88088, 243498560, 93093]

  let inv_after = 774400040
  let lp_balance_after = lp_balance_before
  let (output_pool_data, output_value) =
    t2t_pool_state(
      n,
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
    t2t_pool_state(
      n,
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
    t2t_pool_state(
      n,
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
  let inv_after_wrong = inv_before - 1
  // Validations:
  validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data,
    [inv_after, base_ind, quote_ind],
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value_wrong_balances,
    output_pool_data,
    [inv_after, base_ind, quote_ind],
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_fees,
    [inv_after, base_ind, quote_ind],
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data,
    [inv_after_wrong, base_ind, quote_ind],
  )
}

test test_swap_to_min_decimals_fixtures() {
  // 0 -> 1
  let base_ind = 0
  let quote_ind = 1
  let n = 3
  let a = 300
  let an2n = a * math.pow(n, 2 * n)
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
    t2t_pool_state(
      n,
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
    [390088, 468863264, 348093]

  let collected_protocol_fees_after =
    [88088, 230228480, 93093]

  let inv_after = 5532477203
  let lp_balance_after = lp_balance_before
  let (output_pool_data, output_value) =
    t2t_pool_state(
      n,
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
    t2t_pool_state(
      n,
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
    t2t_pool_state(
      n,
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
  let inv_after_wrong = inv_before - 1
  // Validations:
  validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data,
    [inv_after, base_ind, quote_ind],
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value_wrong_balances,
    output_pool_data,
    [inv_after, base_ind, quote_ind],
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data_wrong_fees,
    [inv_after, base_ind, quote_ind],
  ) && !validate_amm_operation(
    input_value,
    input_pool_data,
    output_value,
    output_pool_data,
    [inv_after_wrong, base_ind, quote_ind],
  )
}
