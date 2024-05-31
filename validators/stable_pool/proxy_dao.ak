use aiken/list
use aiken/math
use aiken/transaction.{
  InlineDatum, Input, Output, ScriptContext, Spend, Transaction,
}
use aiken/transaction/credential.{StakeCredential}
use aiken/transaction/value.{Value}
use splash/plutus.{Asset}
use splash/stableswap/test_utils.{
  generate_tradable_assets, lp_emission, make_proxy_dao_tx_context,
  make_proxy_dao_tx_context_with_new_pool_stake_cred, n2t_pool_state,
  proxy_dao_state, t2t_pool_state,
}
use splash/stableswap/types.{DAOAction, DAOData, PoolData}
use splash/stableswap/utils

// Limits of the adjustable parameters:

const ampl_coeff_min = 1

const ampl_coeff_max = 10_000

const lp_fee_num_min = 1

const lp_fee_num_max = 20_000

const protocol_fee_num_min = 1

const protocol_fee_num_max = 99_999

// DAO stable pool validator:
validator {
  fn stable_proxy_dao(
    datum: DAOData,
    action: DAOAction,
    ctx: ScriptContext,
  ) -> Bool {
    let ScriptContext { transaction, purpose } = ctx
    expect Spend(dao_spent_utxo_reference) = purpose
    let Transaction { inputs, outputs, .. } = transaction
    let DAOAction {
      pool_in_ix,
      pool_out_ix,
      dao_in_ix,
      dao_out_ix,
      dao_action_ix,
    } = action
    // ==================================== INPUTS ==================================== //
    // Pool input:
    expect Some(pool_input) = list.at(inputs, pool_in_ix)
    expect Output {
      value: pool_in_value,
      address: pool_in_address,
      datum: InlineDatum(pool_in_datum),
      ..
    } = pool_input.output
    expect Some(pool_in_stake_cred) = pool_in_address.stake_credential
    expect input_datum: PoolData = pool_in_datum
    // DAO input:
    expect Some(dao_input) = list.at(inputs, dao_in_ix)
    let Output { value: dao_in_value, address: dao_in_addr, .. } =
      dao_input.output

    // ==================================== OUTPUTS ==================================== //
    // Pool output:
    expect Some(pool_out_output) = list.at(outputs, pool_out_ix)
    expect Output {
      value: pool_out_value,
      address: pool_out_address,
      datum: InlineDatum(pool_out_datum),
      ..
    } = pool_out_output
    expect Some(pool_out_stake_cred) = pool_out_address.stake_credential

    expect output_datum: PoolData = pool_out_datum

    // DAO output:
    expect Some(dao_out_output) = list.at(outputs, dao_out_ix)
    expect Output {
      value: dao_out_value,
      address: dao_out_addr,
      datum: InlineDatum(dao_out_datum),
      ..
    } = dao_out_output
    expect proxy_dao_out_datum: DAOData = dao_out_datum

    // ==================================== DAO SELF VALIDATIONS ==================================== //
    // 1. DAO input is valid:
    let valid_dao_input: Bool =
      dao_spent_utxo_reference == dao_input.output_reference

    // 2. DAO credentials are preserved:
    let proxy_dao_credentials_preserved: Bool = dao_in_addr == dao_out_addr

    // 3. DAO config params are preserved:
    let proxy_dao_congif_preserved: Bool = datum == proxy_dao_out_datum

    // 4. No extra tokens in the DAO's value (Only ADA can be in the DAO's value):
    let no_more_tokens_in_proxy_dao_out: Bool = dao_in_value == dao_out_value

    // 5. DAO action is valid:
    let valid_dao_action: Bool =
      validate_stable_proxy_dao_action(
        pool_in_value,
        pool_in_stake_cred,
        input_datum,
        pool_out_value,
        pool_out_stake_cred,
        output_datum,
        dao_action_ix,
      )

    valid_dao_input && proxy_dao_credentials_preserved && proxy_dao_congif_preserved && no_more_tokens_in_proxy_dao_out && valid_dao_action
  }
}

fn validate_stable_proxy_dao_action(
  input_value: Value,
  input_stake_cred: StakeCredential,
  input_datum: PoolData,
  output_value: Value,
  output_stake_cred: StakeCredential,
  output_datum: PoolData,
  dao_action_ix: Int,
) -> Bool {
  // ==================================== INPUT DATA ==================================== //
  // Pool adjustable params| Input:
  let PoolData {
    n,
    an2n: an2n0,
    lp_fee_num: lp_fee_num0,
    ampl_coeff_is_editable,
    lp_fee_is_editable,
    protocol_fee_num: protocol_fee_num0,
    treasury_address: treasury_addr0,
    protocol_fees: protocol_fees0,
    alpha: alpha0,
    betta: betta0,
    lp_token,
    ..
  } = input_datum
  let Asset { policy: lp_token_policy, name: lp_token_name } = lp_token
  // Liquidity tokens | Input:
  let lp_amount0 =
    value.quantity_of(input_value, lp_token_policy, lp_token_name)

  // ==================================== OUTPUT DATA ==================================== //
  // Pool adjustable params| Output:
  let PoolData {
    an2n: an2n1,
    lp_fee_num: lp_fee_num1,
    protocol_fee_num: protocol_fee_num1,
    treasury_address: treasury_addr1,
    protocol_fees: protocol_fees1,
    alpha: alpha1,
    betta: betta1,
    ..
  } = output_datum
  // Liquidity tokens | Output:
  let lp_amount1 =
    value.quantity_of(output_value, lp_token_policy, lp_token_name)

  // 1. Pool invariant value is preserved:
  let invatiant_preserved: Bool = alpha0 == alpha1 && betta0 == betta1
  // 2. LP tokens are preserved:
  let lp_token_preserved: Bool = lp_amount0 == lp_amount1

  // ==================================== DAO ACTION VALIDATIONS ==================================== //
  // 3. Action is valid:
  let valid_dao_action =
    if dao_action_ix == 0 {
      // Update liquidity provider fee:
      let liquidity_provider_fee_inside_bounds: Bool =
        lp_fee_num1 >= lp_fee_num_min && lp_fee_num1 <= lp_fee_num_max
      let params_preserved_for_liquidity_provider_fee_num_update: Bool =
        protocol_fees0 == protocol_fees1 && input_value == output_value && input_stake_cred == output_stake_cred && an2n0 == an2n1 && protocol_fee_num0 == protocol_fee_num1 && treasury_addr0 == treasury_addr1

      lp_fee_is_editable && liquidity_provider_fee_inside_bounds && params_preserved_for_liquidity_provider_fee_num_update
    } else if
    dao_action_ix == 1{
    
      // Update protocol fee num:
      let protocol_fee_num_inside_bounds: Bool =
        protocol_fee_num1 >= protocol_fee_num_min && protocol_fee_num1 <= protocol_fee_num_max

      let params_preserved_for_protocol_fee_num_update: Bool =
        protocol_fees0 == protocol_fees1 && input_value == output_value && input_stake_cred == output_stake_cred && an2n0 == an2n1 && lp_fee_num1 == lp_fee_num0 && treasury_addr1 == treasury_addr0
      protocol_fee_num_inside_bounds && params_preserved_for_protocol_fee_num_update
    } else if
    dao_action_ix == 2{
    
      // Update treasury address:
      let params_preserved_for_treasury_addr_update: Bool =
        protocol_fees0 == protocol_fees1 && input_value == output_value && input_stake_cred == output_stake_cred && an2n0 == an2n1 && lp_fee_num1 == lp_fee_num0 && protocol_fee_num1 == protocol_fee_num0
      params_preserved_for_treasury_addr_update
    } else if dao_action_ix == 3 {
      let PoolData { tradable_assets, .. } = input_datum
      // Withdrawn protocol fees:
      let total_balances0 =
        utils.extract_target_balances(tradable_assets, input_value)
      let total_balances1 =
        utils.extract_target_balances(tradable_assets, output_value)
      let total_reserves_deltas =
        utils.lists_diff(total_balances1, total_balances0)

      let collected_protocol_fees_deltas =
        utils.lists_diff(protocol_fees1, protocol_fees0)
      let valid_fees_amount_withdrawned: Bool =
        total_reserves_deltas == collected_protocol_fees_deltas

      let params_preserved_for_protocol_fees_withdrawn: Bool =
        input_stake_cred == output_stake_cred && an2n0 == an2n1 && lp_fee_num1 == lp_fee_num0 && protocol_fee_num1 == protocol_fee_num0 && treasury_addr1 == treasury_addr0
      params_preserved_for_protocol_fees_withdrawn && valid_fees_amount_withdrawned
    } else if
    dao_action_ix == 4{
    
      // Update 'ampl_coeff' value of the pool.
      // NB: 'ampl_coeff' can be updated only for balanced pool,
      // where invariant value doesn't depend on 'an2n', the 'invatiant_preserved' flag guarantees this.
      let n2n = math.pow(n, 2 * n)
      let ampl_coeff1 = an2n1 / n2n

      let ampl_coeff_inside_bounds: Bool =
        ampl_coeff1 >= ampl_coeff_min && ampl_coeff1 <= ampl_coeff_max
      let params_preserved_for_ampl_coeff_update: Bool =
        protocol_fees0 == protocol_fees1 && input_value == output_value && input_stake_cred == output_stake_cred && lp_fee_num1 == lp_fee_num0 && protocol_fee_num1 == protocol_fee_num0 && treasury_addr1 == treasury_addr0
      ampl_coeff_is_editable && ampl_coeff_inside_bounds && params_preserved_for_ampl_coeff_update
    } else if
    dao_action_ix == 5{
    
      // Update staking credential of the pool:
      let params_preserved_for_staking_credential_update: Bool =
        protocol_fees0 == protocol_fees1 && input_value == output_value && an2n0 == an2n1 && lp_fee_num1 == lp_fee_num0 && protocol_fee_num1 == protocol_fee_num0 && treasury_addr1 == treasury_addr0
      params_preserved_for_staking_credential_update
    } else {
      False
    }
  invatiant_preserved && lp_token_preserved && valid_dao_action
}

test test_dao_params_update() {
  // Update here means update to the same value.
  let n = 3
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
  let an2n = a_before * math.pow(n, 2 * n)
  let lp_fee_num_before = 100
  let protocol_fee_num_before = 100

  let inv_before = default_token_value * n
  let lp_balance_before = lp_emission - inv_before

  let (input_pool_data, input_pool_value) =
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
  let (proxy_dao_data, proxy_dao_value) =
    proxy_dao_state(@"pool_nft_policy", @"pool_nft_name")

  let ctx =
    make_proxy_dao_tx_context(
      proxy_dao_data,
      proxy_dao_value,
      proxy_dao_data,
      proxy_dao_value,
      input_pool_data,
      input_pool_value,
      input_pool_data,
      input_pool_value,
    )

  let dao_action_ix_list =
    [0, 1, 2, 3, 4, 5]

  let validation_results =
    list.map(
      dao_action_ix_list,
      fn(a_ix) {
        let redeemer = DAOAction(0, 0, 1, 1, a_ix)
        stable_proxy_dao(proxy_dao_data, redeemer, ctx)
      },
    )
  let n_cases = list.length(dao_action_ix_list)
  list.count(validation_results, fn(r) { r == True }) == n_cases
}

test test_update_stake_cred() {
  let n = 3
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
  let an2n = a_before * math.pow(n, 2 * n)

  let lp_fee_num_before = 100
  let protocol_fee_num_before = 100

  let inv_before = default_token_value * n
  let lp_balance_before = lp_emission - inv_before

  let tradable_assets_n2t = list.drop(tradable_assets, 1)
  let tradable_assets_n2t =
    list.push(tradable_assets_n2t, Asset { policy: #"", name: #"" })

  let (input_pool_data_n2t, input_pool_value_n2t) =
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
      an2n,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
    )
  let (proxy_dao_data, proxy_dao_value) =
    proxy_dao_state(@"pool_nft_policy", @"pool_nft_name")

  // Output data:
  let (output_pool_data_n2t, output_pool_value_n2t) =
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
      an2n,
      lp_fee_num_before,
      protocol_fee_num_before,
      @"dao_stable_policy",
      @"tr_policy",
      collected_protocol_fees_before,
    )
  let ctx =
    make_proxy_dao_tx_context_with_new_pool_stake_cred(
      proxy_dao_data,
      proxy_dao_value,
      proxy_dao_data,
      proxy_dao_value,
      input_pool_data_n2t,
      input_pool_value_n2t,
      output_pool_data_n2t,
      output_pool_value_n2t,
    )
  let redeemer = DAOAction(0, 0, 1, 1, 5)
  stable_proxy_dao(proxy_dao_data, redeemer, ctx)
}
