use aiken/list
use aiken/transaction.{ScriptContext, Spend, find_input}
use aiken/transaction/value.{quantity_of}
use whalepools/stable3pool/types.{PoolData}
use whalepools/stable3pool/utils.{check_invariant_extremum, convert_pool_datum}

validator {
  fn stable3_pool(datum: PoolData, ctx: ScriptContext) -> Bool {
    let ScriptContext { transaction, purpose } = ctx
    expect Spend(spent_utxo_reference) = purpose
    // Constants:
    let lp_emission = 9223372036854775807
    let nn = 27
    // Input data:
    expect Some(self_input) =
      find_input(transaction.inputs, spent_utxo_reference)
    expect Some(treasury_input) = list.at(transaction.inputs, 1)

    let self = self_input.output
    let treasury0 = treasury_input.output
    let script0 = self.reference_script

    let treasury_nft0 = datum.treasury_nft
    let base_token0 = datum.base_token
    let quote_token00 = datum.quote_token0
    let quote_token10 = datum.quote_token1
    let lp_token0 = datum.lp_token

    let base_token_dec0 = datum.base_token_dec
    let quote_token0_dec0 = datum.quote_token0_dec
    let quote_token1_dec0 = datum.quote_token1_dec

    let denom0 = datum.denom
    let precision0 = datum.precision

    let provider_fee_num0 = datum.provider_fee_num
    let provider_fee_num_bounds0 = datum.provider_fee_num_bounds
    let protocol_share_num0 = datum.protocol_share_num
    let protocol_share_num_bounds0 = datum.protocol_share_num_bounds

    let a0 = datum.ampl_coeff
    let ampl_coeff_bounds0 = datum.ampl_coeff_bounds
    let inv_no_fees0 = datum.inv_no_fees
    let inv0 = datum.inv

    // Output data:
    expect Some(successor) = list.at(transaction.outputs, 0)
    expect Some(treasury1) = list.at(transaction.outputs, 1)

    let datum_successor: PoolData = convert_pool_datum(successor.datum)

    let script1 = successor.reference_script

    let treasury_nft1 = datum_successor.treasury_nft
    let base_token1 = datum_successor.base_token
    let quote_token01 = datum_successor.quote_token0
    let quote_token11 = datum_successor.quote_token1
    let lp_token1 = datum_successor.lp_token

    let base_token_dec1 = datum_successor.base_token_dec
    let quote_token0_dec1 = datum_successor.quote_token0_dec
    let quote_token1_dec1 = datum_successor.quote_token1_dec

    let denom1 = datum_successor.denom
    let precision1 = datum_successor.precision

    let provider_fee_num1 = datum_successor.provider_fee_num
    let provider_fee_num_bounds1 = datum_successor.provider_fee_num_bounds
    let protocol_share_num1 = datum_successor.protocol_share_num
    let protocol_share_num_bounds1 = datum_successor.protocol_share_num_bounds
    let a1 = datum_successor.ampl_coeff
    let ampl_coeff_bounds1 = datum_successor.ampl_coeff_bounds
    let inv_no_fees1 = datum_successor.inv_no_fees
    let inv1 = datum_successor.inv

    // Calculating deltas.
    // Pool:
    let base_policy = base_token0.policy
    let quote0_policy = quote_token00.policy
    let quote1_policy = quote_token10.policy
    let lp_policy = lp_token0.policy

    let base_amount0 = quantity_of(self.value, base_policy, base_token0.name)
    let base_amount1 =
      quantity_of(successor.value, base_policy, base_token0.name)

    let quote0_amount0 =
      quantity_of(self.value, quote0_policy, quote_token00.name)
    let quote0_amount1 =
      quantity_of(successor.value, quote0_policy, quote_token00.name)

    let quote1_amount0 =
      quantity_of(self.value, quote1_policy, quote_token10.name)
    let quote1_amount1 =
      quantity_of(successor.value, quote1_policy, quote_token10.name)

    let lp_amount0 = quantity_of(self.value, lp_policy, lp_token0.name)
    let lp_amount1 = quantity_of(successor.value, lp_policy, lp_token0.name)

    let delta_base = base_amount1 - base_amount0
    let delta_quote0 = quote0_amount1 - quote0_amount0
    let delta_quote1 = quote1_amount1 - quote1_amount0
    let delta_lp = lp_amount1 - lp_amount0
    let delta_inv_no_fees = inv_no_fees1 - inv_no_fees0
    let delta_inv = inv1 - inv0

    // Treasury:
    let treasury_base_amount0 =
      quantity_of(treasury0.value, base_policy, base_token0.name)
    let treasury_base_amount1 =
      quantity_of(treasury1.value, base_policy, base_token0.name)

    let treasury_quote0_amount0 =
      quantity_of(treasury0.value, quote0_policy, quote_token00.name)
    let treasury_quote0_amount1 =
      quantity_of(treasury1.value, quote0_policy, quote_token00.name)

    let treasury_quote1_amount0 =
      quantity_of(treasury0.value, quote1_policy, quote_token10.name)
    let treasury_quote1_amount1 =
      quantity_of(treasury1.value, quote1_policy, quote_token10.name)
    let treasury_delta_base = treasury_base_amount1 - treasury_base_amount0
    let treasury_delta_quote0 =
      treasury_quote0_amount1 - treasury_quote0_amount0
    let treasury_delta_quote1 =
      treasury_quote1_amount1 - treasury_quote1_amount0

    // Validations:
    // 1. Immutable pool configuration parameters are preserved:
    let treasury_preserved = treasury_nft0 == treasury_nft1
    let config_tokens_preserved =
      base_token0 == base_token1 && quote_token00 == quote_token01 && quote_token10 == quote_token11 && lp_token0 == lp_token1
    let config_tokens_decimals_preserved =
      base_token_dec0 == base_token_dec1 && quote_token0_dec0 == quote_token0_dec1 && quote_token1_dec0 == quote_token1_dec1
    let config_precision_preserved =
      precision0 == precision1 && denom0 == denom1
    let config_adj_params_bounds_preserved =
      provider_fee_num_bounds0 == provider_fee_num_bounds1 && protocol_share_num_bounds0 == protocol_share_num_bounds1 && ampl_coeff_bounds0 == ampl_coeff_bounds1

    let preserved_config =
      treasury_preserved && config_tokens_preserved && config_tokens_decimals_preserved && config_precision_preserved && config_adj_params_bounds_preserved

    // 2. Validator script is preserved:
    let preserved_script = script0 == script1

    // 3. Treasury is valid:
    let valid_treasury =
      quantity_of(
        treasury1.value,
        datum.treasury_nft.policy,
        datum.treasury_nft.name,
      ) == 1

    // 4. Action is valid:
    let voting_params_preserved =
      a0 == a1 && provider_fee_num0 == provider_fee_num1 && protocol_share_num0 == protocol_share_num1
    let valid_action =
      if !voting_params_preserved {
        // 4.1. Mmutable parameters are changed by voting.
        // 4.1.1. Valid voting results:
        // TODO: how to check valid voting?
        let valid_voting = False

        // 4.1.2. New parameters are inside their bounds:
        expect Some(lower_a_bound) = list.at(ampl_coeff_bounds0, 0)
        expect Some(upper_a_bound) = list.at(ampl_coeff_bounds0, 1)

        expect Some(lower_provider_fee_num_bound) =
          list.at(provider_fee_num_bounds0, 0)
        expect Some(upper_provider_fee_num_bound) =
          list.at(provider_fee_num_bounds0, 1)

        expect Some(lower_protocol_share_num_bound) =
          list.at(protocol_share_num_bounds0, 0)
        expect Some(upper_protocol_share_num_bound) =
          list.at(protocol_share_num_bounds0, 1)

        let inside_bounds =
          provider_fee_num1 >= lower_provider_fee_num_bound && provider_fee_num1 <= upper_provider_fee_num_bound && (
            protocol_share_num1 >= lower_protocol_share_num_bound && protocol_share_num1 <= upper_protocol_share_num_bound
          ) && ( a1 >= lower_a_bound && a1 <= upper_a_bound )

        // 4.1.3. Reserves are preserved:
        let preserved_reserves =
          delta_base == 0 && delta_quote0 == 0 && delta_quote1 == 0 && delta_lp == 0

        valid_voting && inside_bounds && preserved_reserves && delta_inv_no_fees == 0 && delta_inv_no_fees == 0 && delta_inv == 0
      } else {
        // Deposit/Redeem/Swap
        // Convert all amounts to the common precision:
        let base_amount1_calc = precision0 * base_amount1 / base_token_dec0
        let quote0_amount1_calc =
          precision0 * quote0_amount1 / quote_token0_dec0
        let quote1_amount1_calc =
          precision0 * quote1_amount1 / quote_token1_dec0
        // Check the invariant:
        let ann = a0 * nn

        let valid_invariant_extremum: Bool =
          check_invariant_extremum(
            x_calc: base_amount1_calc,
            y_calc: quote0_amount1_calc,
            z_calc: quote1_amount1_calc,
            d: inv1,
            nn: nn,
            ann: ann,
          )

        // Calculate total deltas (with fees in the treasury):
        let base_total1 = base_amount1 + treasury_delta_base
        let quote0_total1 = quote0_amount1 + treasury_delta_quote0
        let quote1_total1 = quote1_amount1 + treasury_delta_quote1

        let base_amount1_total_calc = precision0 * base_total1 / base_token_dec0
        let quote0_amount1_total_calc =
          precision0 * quote0_total1 / quote_token0_dec0
        let quote1_amount1_total_calc =
          precision0 * quote1_total1 / quote_token1_dec0

        // Check the no-fees invariant error:
        let valid_no_fees_invariant_extremum: Bool =
          check_invariant_extremum(
            x_calc: base_amount1_total_calc,
            y_calc: quote0_amount1_total_calc,
            z_calc: quote1_amount1_total_calc,
            d: inv_no_fees1,
            nn: nn,
            ann: ann,
          )
        if delta_lp != 0 {
          // 4.2. Deposit/Redeem
          let sign_unit =
            if delta_lp < 0 {
              1
            } else {
              -1
            }

          // Calculate ideal amounts (uniform deltas case):
          let base_ideal = base_amount0 * inv_no_fees1 / inv0
          let quote0_ideal = quote0_amount0 * inv_no_fees1 / inv0
          let quote1_ideal = quote1_amount0 * inv_no_fees1 / inv0

          // Calculate difference from the ideal amounts:
          let base_diff =
            if base_ideal > base_total1 {
              base_ideal - base_total1
            } else {
              base_total1 - base_ideal
            }
          let quote0_diff =
            if quote0_ideal > quote0_total1 {
              quote0_ideal - quote0_total1
            } else {
              quote0_total1 - quote0_ideal
            }
          let quote1_diff =
            if quote1_ideal > quote1_total1 {
              quote1_ideal - quote1_total1
            } else {
              quote1_total1 - quote1_ideal
            }

          // Calculate fees:
          let fee_num = provider_fee_num0 * 3 / 8

          let fees_base = fee_num * base_diff / denom0
          let fees_quote0 = fee_num * quote0_diff / denom0
          let fees_quote1 = fee_num * quote1_diff / denom0

          let protocol_fees_base = fees_base * protocol_share_num0 / denom0
          let protocol_fees_quote0 = fees_quote0 * protocol_share_num0 / denom0
          let protocol_fees_quote1 = fees_quote1 * protocol_share_num0 / denom0
          let final_base1 = base_total1 - protocol_fees_base
          let final_quote0 = quote0_total1 - protocol_fees_quote0
          let final_quote1 = quote1_total1 - protocol_fees_quote1

          let supply_lp0 = lp_emission - lp_amount0
          let delta_supply_lp = ( inv_no_fees1 - inv0 ) * supply_lp0 / inv0

          let valid_protocol_fees =
            treasury_delta_base >= final_base1 - base_amount1 && treasury_delta_quote0 >= final_quote0 - quote0_amount1 && treasury_delta_quote1 >= final_quote1 - quote1_amount1

          // 4.2.1. LP tokens delta is valid:
          let valid_lp_delta = sign_unit * delta_lp <= delta_supply_lp
          // 4.2.1. Reserves movement are correct:
          let positive_reserves =
            sign_unit * delta_base >= 0 && sign_unit * delta_quote0 >= 0 && sign_unit * delta_quote1 >= 0

          valid_invariant_extremum && valid_no_fees_invariant_extremum && valid_protocol_fees && valid_lp_delta && positive_reserves
        } else {
          // 4.3. Swap
          let (valid_protocol_fees, valid_quote_deltas) =
            // Swap Y/Z to X:
            if delta_base < 0 {
              (
                treasury_delta_base >= -delta_base * provider_fee_num0 * protocol_share_num0 / (
                  2 * denom0
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
                treasury_delta_quote0 >= -delta_quote0 * provider_fee_num0 * protocol_share_num0 / (
                  2 * denom0
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
                treasury_delta_quote1 >= -delta_quote1 * provider_fee_num0 * protocol_share_num0 / (
                  2 * denom0
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

          valid_invariant_extremum && valid_no_fees_invariant_extremum && valid_protocol_fees && valid_quote_deltas && delta_lp == 0 && inv_no_fees1 == inv0
        }
      }

    preserved_config && preserved_script && valid_treasury && valid_action
  }
}
