use aiken/bytearray
use aiken/int
use aiken/list
use aiken/math
use aiken/transaction.{
  InlineDatum, Input, Output, OutputReference, ScriptContext, Transaction,
  TransactionId, WithdrawFrom,
}
use aiken/transaction/credential.{
  Inline, StakeCredential, from_script, with_delegation_script,
}
use aiken/transaction/value.{Value}
use splash/gen.{generate_assets}
use splash/plutus.{Asset}
use splash/stableswap/types.{DAOAction, PoolDataT2T}

// Constant 'n ^ (2 * n)', where 'n' is the number of tradable tokens in the pool:
const n2n = 16

// Limits of the adjustable parameters:
const ampl_coeff_min = 1

const ampl_coeff_max = 10_000

const lp_fee_num_min = 1

const lp_fee_num_max = 20_000

const protocol_fee_num_min = 1

const protocol_fee_num_max = 99_999

const fee_denom = 100_000

// DAO stable pool t2t validator:
validator(ampl_coeff_is_editable: Bool, lp_fee_is_editable: Bool) {
  fn stable_pool_t2t_proxy_dao(action: DAOAction, ctx: ScriptContext) -> Bool {
    let ScriptContext { transaction, purpose } = ctx
    expect WithdrawFrom(_) = purpose
    let Transaction { inputs, outputs, .. } = transaction
    let DAOAction { pool_in_ix, pool_out_ix, dao_action_ix } = action
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
    expect input_datum: PoolDataT2T = pool_in_datum

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

    expect output_datum: PoolDataT2T = pool_out_datum

    // ==================================== DAO SELF VALIDATIONS ==================================== //
    // 1. Valid pool:
    let Asset { policy: pool_nft_policy, name: pool_nft_name } =
      input_datum.pool_nft
    let valid_pool: Bool =
      value.quantity_of(pool_in_value, pool_nft_policy, pool_nft_name) == 1 && value.quantity_of(
        pool_out_value,
        pool_nft_policy,
        pool_nft_name,
      ) == 1

    // 2. DAO action is valid:
    let valid_dao_action: Bool =
      validate_stable_pool_t2t_proxy_dao_action(
        pool_in_value,
        pool_in_stake_cred,
        input_datum,
        pool_out_value,
        pool_out_stake_cred,
        output_datum,
        dao_action_ix,
        ampl_coeff_is_editable,
        lp_fee_is_editable,
      )

    valid_pool && valid_dao_action
  }
}

fn validate_stable_pool_t2t_proxy_dao_action(
  input_value: Value,
  input_stake_cred: StakeCredential,
  input_datum: PoolDataT2T,
  output_value: Value,
  output_stake_cred: StakeCredential,
  output_datum: PoolDataT2T,
  dao_action_ix: Int,
  ampl_coeff_is_editable: Bool,
  lp_fee_is_editable: Bool,
) -> Bool {
  // ==================================== INPUT DATA ==================================== //
  // Pool adjustable params| Input:
  let PoolDataT2T {
    an2n: an2n0,
    lp_fee_num: lp_fee_num0,
    protocol_fee_num: protocol_fee_num0,
    treasury_address: treasury_addr0,
    protocol_fees_x: protocol_fees_x0,
    protocol_fees_y: protocol_fees_y0,
    lp_token,
    ..
  } = input_datum
  let Asset { policy: lp_token_policy, name: lp_token_name } = lp_token
  // Liquidity tokens | Input:
  let lp_amount0 =
    value.quantity_of(input_value, lp_token_policy, lp_token_name)

  // ==================================== OUTPUT DATA ==================================== //
  // Pool's mutable params | Output:
  let PoolDataT2T {
    an2n: an2n1,
    lp_fee_num: lp_fee_num1,
    protocol_fee_num: protocol_fee_num1,
    treasury_address: treasury_addr1,
    protocol_fees_x: protocol_fees_x1,
    protocol_fees_y: protocol_fees_y1,
    ..
  } = output_datum
  // Liquidity tokens | Output:
  let lp_amount1 =
    value.quantity_of(output_value, lp_token_policy, lp_token_name)

  // 1. LP tokens are preserved:
  let lp_token_preserved: Bool = lp_amount0 == lp_amount1

  // ==================================== DAO ACTION VALIDATIONS ==================================== //
  // 2. Action is valid:
  let valid_dao_action =
    if dao_action_ix == 0 {
      // Update liquidity provider fee:
      let liquidity_provider_fee_inside_bounds: Bool =
        lp_fee_num1 >= lp_fee_num_min && lp_fee_num1 <= lp_fee_num_max
      let params_preserved_for_liquidity_provider_fee_num_update: Bool =
        protocol_fees_x0 == protocol_fees_x1 && protocol_fees_y0 == protocol_fees_y1 && input_value == output_value && input_stake_cred == output_stake_cred && an2n0 == an2n1 && protocol_fee_num0 == protocol_fee_num1 && treasury_addr0 == treasury_addr1
      let total_fee_inside_bounds_for_liquidity_provider_fee_num_update: Bool =
        lp_fee_num1 + protocol_fee_num1 < fee_denom
      lp_fee_is_editable && liquidity_provider_fee_inside_bounds && params_preserved_for_liquidity_provider_fee_num_update && total_fee_inside_bounds_for_liquidity_provider_fee_num_update
    } else if
    dao_action_ix == 1{
    
      // Update protocol fee num:
      let protocol_fee_num_inside_bounds: Bool =
        protocol_fee_num1 >= protocol_fee_num_min && protocol_fee_num1 <= protocol_fee_num_max

      let params_preserved_for_protocol_fee_num_update: Bool =
        protocol_fees_x0 == protocol_fees_x1 && protocol_fees_y0 == protocol_fees_y1 && input_value == output_value && input_stake_cred == output_stake_cred && an2n0 == an2n1 && lp_fee_num1 == lp_fee_num0 && treasury_addr1 == treasury_addr0
      let total_fee_inside_bounds_for_protocol_fee_num_update: Bool =
        lp_fee_num1 + protocol_fee_num1 < fee_denom
      protocol_fee_num_inside_bounds && params_preserved_for_protocol_fee_num_update && total_fee_inside_bounds_for_protocol_fee_num_update
    } else if
    dao_action_ix == 2{
    
      // Update treasury address:
      let params_preserved_for_treasury_addr_update: Bool =
        protocol_fees_x0 == protocol_fees_x1 && protocol_fees_y0 == protocol_fees_y1 && input_value == output_value && input_stake_cred == output_stake_cred && an2n0 == an2n1 && lp_fee_num1 == lp_fee_num0 && protocol_fee_num1 == protocol_fee_num0
      params_preserved_for_treasury_addr_update
    } else if dao_action_ix == 3 {
      let PoolDataT2T { asset_x, asset_y, .. } = input_datum
      let Asset { policy: asset_x_policy, name: asset_x_name } = asset_x
      let Asset { policy: asset_y_policy, name: asset_y_name } = asset_y
      // Withdrawn protocol fees:
      let x_total0 =
        value.quantity_of(input_value, asset_x_policy, asset_x_name)
      let y_total0 =
        value.quantity_of(input_value, asset_y_policy, asset_y_name)
      let x_total1 =
        value.quantity_of(output_value, asset_x_policy, asset_x_name)
      let y_total1 =
        value.quantity_of(output_value, asset_y_policy, asset_y_name)
      let x_total_delta = x_total1 - x_total0
      let y_total_delta = y_total1 - y_total0

      let protocol_fees_x_delta = protocol_fees_x1 - protocol_fees_x0
      let protocol_fees_y_delta = protocol_fees_y1 - protocol_fees_y0
      let valid_fees_amount_withdrawned: Bool =
        x_total_delta == protocol_fees_x_delta && y_total_delta == protocol_fees_y_delta

      let params_preserved_for_protocol_fees_withdrawn: Bool =
        input_stake_cred == output_stake_cred && an2n0 == an2n1 && lp_fee_num1 == lp_fee_num0 && protocol_fee_num1 == protocol_fee_num0 && treasury_addr1 == treasury_addr0
      params_preserved_for_protocol_fees_withdrawn && valid_fees_amount_withdrawned && protocol_fees_x1 >= 0 && protocol_fees_y1 >= 0
    } else if
    dao_action_ix == 4{
    
      let PoolDataT2T { asset_x, asset_y, multiplier_x, multiplier_y, .. } =
        input_datum
      let Asset { policy: asset_x_policy, name: asset_x_name } = asset_x
      let Asset { policy: asset_y_policy, name: asset_y_name } = asset_y
      let x_total0 =
        value.quantity_of(input_value, asset_x_policy, asset_x_name)
      let y_total0 =
        value.quantity_of(input_value, asset_y_policy, asset_y_name)
      let x_tradable0 = x_total0 - protocol_fees_x0
      let y_tradable0 = y_total0 - protocol_fees_y0

      // Update 'an2n' value of the pool.
      // NB: 'an2n' can be updated only for balanced pool,
      // where invariant value doesn't depend on 'a', the 'invatiant_preserved' flag guarantees this.
      let ampl_coeff1 = an2n1 / n2n

      let ampl_coeff_inside_bounds: Bool =
        ampl_coeff1 >= ampl_coeff_min && ampl_coeff1 <= ampl_coeff_max
      let params_preserved_for_ampl_coeff_update: Bool =
        protocol_fees_x0 == protocol_fees_x1 && protocol_fees_y0 == protocol_fees_y1 && input_value == output_value && input_stake_cred == output_stake_cred && lp_fee_num1 == lp_fee_num0 && protocol_fee_num1 == protocol_fee_num0 && treasury_addr1 == treasury_addr0
      let invatiant_preserved: Bool =
        x_tradable0 / multiplier_y == y_tradable0 / multiplier_x

      ampl_coeff_is_editable && ampl_coeff_inside_bounds && params_preserved_for_ampl_coeff_update && invatiant_preserved
    } else if
    dao_action_ix == 5{
    
      // Update staking credential of the pool:
      let params_preserved_for_staking_credential_update: Bool =
        protocol_fees_x0 == protocol_fees_x1 && protocol_fees_y0 == protocol_fees_y1 && input_value == output_value && an2n0 == an2n1 && lp_fee_num1 == lp_fee_num0 && protocol_fee_num1 == protocol_fee_num0 && treasury_addr1 == treasury_addr0
      params_preserved_for_staking_credential_update
    } else {
      False
    }
  lp_token_preserved && valid_dao_action
}

// ================================================= TESTS ================================================= //

// ============================================ COMMON TEMPLATES ============================================ //
const default_lovelace_value = 1_000_000_000_000_000

const lp_emission = 9223372036854775807

/// Returns datum and value with a given params for t2t pool:
fn t2t_pool_state(
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
) -> (PoolDataT2T, Value) {
  // Calculate multipliers:
  let tradable_decimals_sorted = list.sort(tradable_decimals, int.compare)
  expect Some(precision) = list.last(tradable_decimals_sorted)
  let tradable_tokens_multipliers =
    list.map(tradable_decimals, fn(x) { precision / x })
  // Make datum:
  expect Some(asset_x) = list.at(tradable_assets, 0)
  expect Some(asset_y) = list.at(tradable_assets, 1)
  expect Some(multiplier_x) = list.at(tradable_tokens_multipliers, 0)
  expect Some(multiplier_y) = list.at(tradable_tokens_multipliers, 1)
  expect Some(protocol_fees_x) = list.at(protocol_fees, 0)
  expect Some(protocol_fees_y) = list.at(protocol_fees, 1)
  let pool_data =
    PoolDataT2T {
      pool_nft: Asset {
        policy: bytearray.from_string(pool_nft_policy_string),
        name: bytearray.from_string(pool_nft_name_string),
      },
      asset_x,
      asset_y,
      multiplier_x,
      multiplier_y,
      lp_token: Asset {
        policy: bytearray.from_string(lp_policy_string),
        name: bytearray.from_string(lp_name_string),
      },
      an2n,
      lp_fee_num,
      protocol_fee_num,
      dao_stabe_proxy_witness: bytearray.from_string(dao_policy_string),
      treasury_address: bytearray.from_string(treasury_address_string),
      protocol_fees_x,
      protocol_fees_y,
    }
  // Make value:
  expect Some(balance_x) = list.at(balances, 0)
  expect Some(balance_y) = list.at(balances, 1)

  let input_value =
    value.from_lovelace(default_lovelace_value)
      |> value.add(pool_data.pool_nft.policy, pool_data.pool_nft.name, 1)
      |> value.add(pool_data.lp_token.policy, pool_data.lp_token.name, lp_balance)
      |> value.add(asset_x.policy, asset_x.name, balance_x)
      |> value.add(asset_y.policy, asset_y.name, balance_y)

  (pool_data, input_value)
}

/// Returns PoolDataT2T and Value with a given params for n2t pool:
fn n2t_pool_state(
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
) -> (PoolDataT2T, Value) {
  // Calculate multipliers:
  let tradable_decimals_sorted = list.sort(tradable_decimals, int.compare)
  expect Some(precision) = list.last(tradable_decimals_sorted)
  let tradable_tokens_multipliers =
    list.map(tradable_decimals, fn(x) { precision / x })
  // Make datum:
  expect Some(asset_x) = list.at(tradable_assets, 0)
  expect Some(asset_y) = list.at(tradable_assets, 1)
  expect Some(multiplier_x) = list.at(tradable_tokens_multipliers, 0)
  expect Some(multiplier_y) = list.at(tradable_tokens_multipliers, 1)
  expect Some(protocol_fees_x) = list.at(protocol_fees, 0)
  expect Some(protocol_fees_y) = list.at(protocol_fees, 1)
  let pool_data =
    PoolDataT2T {
      pool_nft: Asset {
        policy: bytearray.from_string(pool_nft_policy_string),
        name: bytearray.from_string(pool_nft_name_string),
      },
      asset_x,
      asset_y,
      multiplier_x,
      multiplier_y,
      lp_token: Asset {
        policy: bytearray.from_string(lp_policy_string),
        name: bytearray.from_string(lp_name_string),
      },
      an2n,
      lp_fee_num,
      protocol_fee_num,
      dao_stabe_proxy_witness: bytearray.from_string(dao_policy_string),
      treasury_address: bytearray.from_string(treasury_address_string),
      protocol_fees_x,
      protocol_fees_y,
    }
  expect Some(asset1) = list.at(tradable_assets, 1)
  expect Some(balance0) = list.at(balances, 0)
  expect Some(balance1) = list.at(balances, 1)

  let input_value =
    value.from_lovelace(balance0)
      |> value.add(pool_data.pool_nft.policy, pool_data.pool_nft.name, 1)
      |> value.add(pool_data.lp_token.policy, pool_data.lp_token.name, lp_balance)
      |> value.add(asset1.policy, asset1.name, balance1)

  (pool_data, input_value)
}

/// Returns proxy-DAO state transition context:
pub fn make_proxy_dao_tx_context(
  input_pool_data: PoolDataT2T,
  input_pool_value: Value,
  output_pool_data: PoolDataT2T,
  output_pool_value: Value,
) -> ScriptContext {
  let pool_addr =
    from_script(#"f4c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50")
  let pool_addr_before = with_delegation_script(pool_addr, #"00")
  let pool_addr_after = pool_addr_before

  let pool_ref_script =
    Some(#"ee7659342d952e4569acc0592e703737530d1e79fe932673a0fa3e50")
  let proxy_dao_addr =
    from_script(#"f2c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50")
  let pool_out_utxo_ref =
    OutputReference { transaction_id: TransactionId(""), output_index: 0 }
  let pool_input_utxo =
    Output {
      address: pool_addr_before,
      value: input_pool_value,
      datum: InlineDatum(input_pool_data),
      reference_script: pool_ref_script,
    }

  let pool_output_utxo =
    Output {
      address: pool_addr_after,
      value: output_pool_value,
      datum: InlineDatum(output_pool_data),
      reference_script: pool_ref_script,
    }

  let pool_input =
    Input { output_reference: pool_out_utxo_ref, output: pool_input_utxo }

  let cred = Inline(proxy_dao_addr.payment_credential)
  ScriptContext {
    purpose: WithdrawFrom(cred),
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

/// Returns proxy-DAO state transition context for update pool staking cred. action:
pub fn make_proxy_dao_tx_context_with_new_pool_stake_cred(
  input_pool_data: PoolDataT2T,
  input_pool_value: Value,
  output_pool_data: PoolDataT2T,
  output_pool_value: Value,
) -> ScriptContext {
  let pool_addr =
    from_script(#"f4c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50")
  let pool_addr_before = with_delegation_script(pool_addr, #"00")
  let pool_addr_after = with_delegation_script(pool_addr, #"01")

  let pool_ref_script =
    Some(#"ee7659342d952e4569acc0592e703737530d1e79fe932673a0fa3e50")
  let proxy_dao_addr =
    from_script(#"f2c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50")
  let pool_out_utxo_ref =
    OutputReference { transaction_id: TransactionId(""), output_index: 0 }
  let pool_input_utxo =
    Output {
      address: pool_addr_before,
      value: input_pool_value,
      datum: InlineDatum(input_pool_data),
      reference_script: pool_ref_script,
    }

  let pool_output_utxo =
    Output {
      address: pool_addr_after,
      value: output_pool_value,
      datum: InlineDatum(output_pool_data),
      reference_script: pool_ref_script,
    }

  let pool_input =
    Input { output_reference: pool_out_utxo_ref, output: pool_input_utxo }

  let cred = Inline(proxy_dao_addr.payment_credential)

  ScriptContext {
    purpose: WithdrawFrom(cred),
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

// ================================================= TESTS DAO ACTIONS ================================================= //

test test_dao_params_update() {
  // Update here means update to the same value.
  let n = 2
  let min_value = 1_000_000
  let default_decimal = 1_000_000
  let default_token_value = min_value * default_decimal

  let tradable_assets =
    generate_assets(@"some_pool_token", @"some_pool_token_name", n)

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

  let ctx =
    make_proxy_dao_tx_context(
      input_pool_data,
      input_pool_value,
      input_pool_data,
      input_pool_value,
    )

  let (_, input_pool_value_false) =
    t2t_pool_state(
      total_balances_before,
      lp_balance_before,
      @"pool_nft_policy_wrong",
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

  let ctx_false =
    make_proxy_dao_tx_context(
      input_pool_data,
      input_pool_value,
      input_pool_data,
      input_pool_value_false,
    )

  let dao_action_ix_list =
    [0, 1, 2, 3, 4, 5]

  let validation_results =
    list.map(
      dao_action_ix_list,
      fn(a_ix) {
        let redeemer = DAOAction(0, 0, a_ix)
        stable_pool_t2t_proxy_dao(True, True, redeemer, ctx)
      },
    )

  let validation_results_false =
    list.map(
      dao_action_ix_list,
      fn(a_ix) {
        let redeemer = DAOAction(0, 0, a_ix)
        stable_pool_t2t_proxy_dao(True, True, redeemer, ctx_false)
      },
    )
  let n_cases = list.length(dao_action_ix_list)
  list.count(validation_results, fn(r) { r == True }) == n_cases && list.count(
    validation_results_false,
    fn(r) { r == False },
  ) == n_cases
}

test test_update_stake_cred() {
  let n = 2
  let min_value = 1_000_000
  let default_decimal = 1_000_000
  let default_token_value = min_value * default_decimal

  let tradable_assets =
    generate_assets(@"some_pool_token", @"some_pool_token_name", n)

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

  // Output data:
  let (output_pool_data_n2t, output_pool_value_n2t) =
    n2t_pool_state(
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
      input_pool_data_n2t,
      input_pool_value_n2t,
      output_pool_data_n2t,
      output_pool_value_n2t,
    )
  let redeemer = DAOAction(0, 0, 5)
  stable_pool_t2t_proxy_dao(True, True, redeemer, ctx)
}
