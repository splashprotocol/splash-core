use aiken/list
use aiken/transaction.{
  InlineDatum, Input, Output, ScriptContext, Spend, Transaction,
}
use aiken/transaction/credential.{StakeCredential}
use aiken/transaction/value.{Value, lovelace_of}
use splash/plutus.{Asset}
use splash/stableswap/types.{DAOAction, DAODatum, PoolData}
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
  fn stable_pool_dao(
    datum: DAODatum,
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

    // DAO input params:
    let dao_in_tokens_policies = value.policies(dao_in_value)

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
    expect proxy_dao_out_datum: DAODatum = dao_out_datum

    // DAO output params:
    let dao_out_tokens_policies = value.policies(dao_out_value)

    // ==================================== DAO SELF VALIDATIONS ==================================== //
    // 1. DAO input is valid:
    let valid_dao_input: Bool =
      dao_spent_utxo_reference == dao_input.output_reference

    // 2. DAO credentials are preserved:
    let proxy_dao_credentials_preserved: Bool = dao_in_addr == dao_out_addr

    // 3. DAO config params are preserved:
    let proxy_dao_congif_preserved: Bool = datum == proxy_dao_out_datum

    // 4. No extra tokens in the DAO's value (Only ADA can be in the DAO's value):
    let no_more_tokens_in_proxy_dao_out: Bool =
      dao_in_tokens_policies == dao_out_tokens_policies && list.length(
        dao_out_tokens_policies,
      ) == 1

    // 5. DAO action is valid:
    let valid_dao_action: Bool =
      validate_stable_pool_dao_action(
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

fn validate_stable_pool_dao_action(
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
    tradable_assets,
    ampl_coeff: ampl_coeff0,
    lp_fee_num: lp_fee_num0,
    protocol_fee_num: protocol_fee_num0,
    dao_stabe_proxy_witness: dao_stabe_proxy_witness0,
    treasury_address: treasury_addr0,
    protocol_fees: protocol_fees0,
    inv: inv0,
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
    ampl_coeff: ampl_coeff1,
    lp_fee_num: lp_fee_num1,
    protocol_fee_num: protocol_fee_num1,
    dao_stabe_proxy_witness: dao_stabe_proxy_witness1,
    treasury_address: treasury_addr1,
    protocol_fees: protocol_fees1,
    inv: inv1,
    ..
  } = output_datum
  // Liquidity tokens | Output:
  let lp_amount1 =
    value.quantity_of(output_value, lp_token_policy, lp_token_name)

  // 1. Pool invariant value is preserved:
  let invatiant_preserved: Bool = inv0 == inv1
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
        protocol_fees0 == protocol_fees1 && input_value == output_value && input_stake_cred == output_stake_cred && ampl_coeff0 == ampl_coeff1 && protocol_fee_num0 == protocol_fee_num1 && dao_stabe_proxy_witness0 == dao_stabe_proxy_witness1 && treasury_addr0 == treasury_addr1

      liquidity_provider_fee_inside_bounds && params_preserved_for_liquidity_provider_fee_num_update
    } else if
    dao_action_ix == 1{
    
      // Update protocol fee num:
      let protocol_fee_num_inside_bounds: Bool =
        protocol_fee_num1 >= protocol_fee_num_min && protocol_fee_num1 <= protocol_fee_num_max

      let params_preserved_for_protocol_fee_num_update: Bool =
        protocol_fees0 == protocol_fees1 && input_value == output_value && input_stake_cred == output_stake_cred && ampl_coeff0 == ampl_coeff1 && lp_fee_num1 == lp_fee_num0 && treasury_addr1 == treasury_addr0 && dao_stabe_proxy_witness1 == dao_stabe_proxy_witness0
      protocol_fee_num_inside_bounds && params_preserved_for_protocol_fee_num_update
    } else if
    dao_action_ix == 2{
    
      // Update treasury address:
      let params_preserved_for_treasury_addr_update: Bool =
        protocol_fees0 == protocol_fees1 && input_value == output_value && input_stake_cred == output_stake_cred && ampl_coeff0 == ampl_coeff1 && lp_fee_num1 == lp_fee_num0 && protocol_fee_num1 == protocol_fee_num0 && dao_stabe_proxy_witness1 == dao_stabe_proxy_witness0
      params_preserved_for_treasury_addr_update
    } else if dao_action_ix == 3 {
      // Withdrawn protocol fees:
      let total_balances0 =
        utils.extract_target_balances(tradable_assets, input_value)
      let total_balances1 =
        utils.extract_target_balances(tradable_assets, output_value)
      let total_reserves_deltas =
        utils.lists_diff(total_balances1, total_balances0)
      let collected_protocol_fees_deltas =
        utils.lists_diff(protocol_fees1, protocol_fees0)

      let total_reserves_deltas =
        if list.length(tradable_assets) == n - 1 {
          // Native to token pool case, need to take into account ADA delta.
          let ada_delta = lovelace_of(output_value) - lovelace_of(input_value)
          list.push(total_reserves_deltas, ada_delta)
        } else {
          total_reserves_deltas
        }
      let valid_fees_amount_withdrawned: Bool =
        total_reserves_deltas == collected_protocol_fees_deltas

      let params_preserved: Bool =
        input_stake_cred == output_stake_cred && ampl_coeff0 == ampl_coeff1 && lp_fee_num1 == lp_fee_num0 && protocol_fee_num1 == protocol_fee_num0 && treasury_addr1 == treasury_addr0 && dao_stabe_proxy_witness1 == dao_stabe_proxy_witness0
      params_preserved && valid_fees_amount_withdrawned
    } else if dao_action_ix == 4 {
      // Update DAO-proxy witness:
      let params_preserved_for_dao_witness_update: Bool =
        protocol_fees0 == protocol_fees1 && input_value == output_value && input_stake_cred == output_stake_cred && ampl_coeff0 == ampl_coeff1 && lp_fee_num1 == lp_fee_num0 && protocol_fee_num1 == protocol_fee_num0 && treasury_addr1 == treasury_addr0
      params_preserved_for_dao_witness_update
    } else if dao_action_ix == 5 {
      // Update 'ampl_coeff' value of the pool.
      // NB: 'ampl_coeff' can be updated only for balanced pool,
      // where invariant value doesn't depend on 'ampl_coeff', the 'invatiant_preserved' flag guarantees this.
      let ampl_coeff_inside_bounds: Bool =
        ampl_coeff1 >= ampl_coeff_min && ampl_coeff1 <= ampl_coeff_max
      let params_preserved_for_ampl_coeff_update: Bool =
        protocol_fees0 == protocol_fees1 && input_value == output_value && input_stake_cred == output_stake_cred && lp_fee_num1 == lp_fee_num0 && protocol_fee_num1 == protocol_fee_num0 && treasury_addr1 == treasury_addr0 && dao_stabe_proxy_witness1 == dao_stabe_proxy_witness0
      ampl_coeff_inside_bounds && params_preserved_for_ampl_coeff_update
    } else if dao_action_ix == 6 {
      // Update staking credential of the pool:
      let params_preserved_for_dao_witness_update: Bool =
        protocol_fees0 == protocol_fees1 && input_value == output_value && ampl_coeff0 == ampl_coeff1 && lp_fee_num1 == lp_fee_num0 && protocol_fee_num1 == protocol_fee_num0 && treasury_addr1 == treasury_addr0 && dao_stabe_proxy_witness1 == dao_stabe_proxy_witness0
      params_preserved_for_dao_witness_update
    } else {
      False
    }
  invatiant_preserved && lp_token_preserved && valid_dao_action
}
