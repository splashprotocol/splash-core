use aiken/list
use aiken/transaction.{
  InlineDatum, Input, Output, ScriptContext, Spend, Transaction,
}
use aiken/transaction/credential.{StakeCredential}
use aiken/transaction/value.{Value}
use splash/plutus.{Asset}
use splash/stableswap/types.{DAOAction, PoolDataT2T2T}

// Constant 'n ^ (2 * n)', where 'n' is the number of tradable tokens in the pool:
const n2n = 729

// Limits of the adjustable parameters:
const ampl_coeff_min = 1

const ampl_coeff_max = 10_000

const lp_fee_num_min = 1

const lp_fee_num_max = 20_000

const protocol_fee_num_min = 1

const protocol_fee_num_max = 99_999

// DAO stable pool t2t2t validator:
validator(pool_nft: Asset) {
  fn stable_pool_t2t2t_proxy_dao(action: DAOAction, ctx: ScriptContext) -> Bool {
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
    expect input_datum: PoolDataT2T2T = pool_in_datum
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

    expect output_datum: PoolDataT2T2T = pool_out_datum

    // DAO output:
    expect Some(dao_out_output) = list.at(outputs, dao_out_ix)
    let Output { value: dao_out_value, address: dao_out_addr, .. } =
      dao_out_output

    // ==================================== DAO SELF VALIDATIONS ==================================== //
    // 1. DAO input is valid:
    let valid_dao_input: Bool =
      dao_spent_utxo_reference == dao_input.output_reference

    // 2. DAO credentials are preserved:
    let proxy_dao_credentials_preserved: Bool = dao_in_addr == dao_out_addr

    // 3. Valid pool:
    let Asset { policy: pool_nft_policy, name: pool_nft_name } = pool_nft
    let valid_pool: Bool =
      value.quantity_of(pool_in_value, pool_nft_policy, pool_nft_name) == 1

    // 4. No extra tokens in the DAO's value (Only ADA can be in the DAO's value):
    let no_more_tokens_in_proxy_dao_out: Bool = dao_in_value == dao_out_value

    // 5. DAO action is valid:
    let valid_dao_action: Bool =
      validate_stable_pool_t2t_proxy_dao_action(
        pool_in_value,
        pool_in_stake_cred,
        input_datum,
        pool_out_value,
        pool_out_stake_cred,
        output_datum,
        dao_action_ix,
      )

    valid_dao_input && proxy_dao_credentials_preserved && valid_pool && no_more_tokens_in_proxy_dao_out && valid_dao_action
  }
}

fn validate_stable_pool_t2t_proxy_dao_action(
  input_value: Value,
  input_stake_cred: StakeCredential,
  input_datum: PoolDataT2T2T,
  output_value: Value,
  output_stake_cred: StakeCredential,
  output_datum: PoolDataT2T2T,
  dao_action_ix: Int,
) -> Bool {
  // ==================================== INPUT DATA ==================================== //
  // Pool's mutable params | Input:
  let PoolDataT2T2T {
    an2n: an2n0,
    lp_fee_num: lp_fee_num0,
    ampl_coeff_is_editable,
    lp_fee_is_editable,
    protocol_fee_num: protocol_fee_num0,
    treasury_address: treasury_addr0,
    protocol_fees_x: protocol_fees_x0,
    protocol_fees_y: protocol_fees_y0,
    protocol_fees_z: protocol_fees_z0,
    lp_token,
    ..
  } = input_datum
  let Asset { policy: lp_token_policy, name: lp_token_name } = lp_token
  // Liquidity tokens | Input:
  let lp_amount0 =
    value.quantity_of(input_value, lp_token_policy, lp_token_name)

  // ==================================== OUTPUT DATA ==================================== //
  // Pool adjustable params| Output:
  let PoolDataT2T2T {
    an2n: an2n1,
    lp_fee_num: lp_fee_num1,
    protocol_fee_num: protocol_fee_num1,
    treasury_address: treasury_addr1,
    protocol_fees_x: protocol_fees_x1,
    protocol_fees_y: protocol_fees_y1,
    protocol_fees_z: protocol_fees_z1,
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
        protocol_fees_x0 == protocol_fees_x1 && protocol_fees_y0 == protocol_fees_y1 && protocol_fees_z0 == protocol_fees_z1 && input_value == output_value && input_stake_cred == output_stake_cred && an2n0 == an2n1 && protocol_fee_num0 == protocol_fee_num1 && treasury_addr0 == treasury_addr1

      lp_fee_is_editable && liquidity_provider_fee_inside_bounds && params_preserved_for_liquidity_provider_fee_num_update
    } else if
    dao_action_ix == 1{
    
      // Update protocol fee num:
      let protocol_fee_num_inside_bounds: Bool =
        protocol_fee_num1 >= protocol_fee_num_min && protocol_fee_num1 <= protocol_fee_num_max

      let params_preserved_for_protocol_fee_num_update: Bool =
        protocol_fees_x0 == protocol_fees_x1 && protocol_fees_y0 == protocol_fees_y1 && protocol_fees_z0 == protocol_fees_z1 && input_value == output_value && input_stake_cred == output_stake_cred && an2n0 == an2n1 && lp_fee_num1 == lp_fee_num0 && treasury_addr1 == treasury_addr0
      protocol_fee_num_inside_bounds && params_preserved_for_protocol_fee_num_update
    } else if
    dao_action_ix == 2{
    
      // Update treasury address:
      let params_preserved_for_treasury_addr_update: Bool =
        protocol_fees_x0 == protocol_fees_x1 && protocol_fees_y0 == protocol_fees_y1 && protocol_fees_z0 == protocol_fees_z1 && input_value == output_value && input_stake_cred == output_stake_cred && an2n0 == an2n1 && lp_fee_num1 == lp_fee_num0 && protocol_fee_num1 == protocol_fee_num0
      params_preserved_for_treasury_addr_update
    } else if dao_action_ix == 3 {
      let PoolDataT2T2T { asset_x, asset_y, asset_z, .. } = input_datum
      let Asset { policy: asset_x_policy, name: asset_x_name } = asset_x
      let Asset { policy: asset_y_policy, name: asset_y_name } = asset_y
      let Asset { policy: asset_z_policy, name: asset_z_name } = asset_z
      // Withdrawn protocol fees:
      let x_total0 =
        value.quantity_of(input_value, asset_x_policy, asset_x_name)
      let y_total0 =
        value.quantity_of(input_value, asset_y_policy, asset_y_name)
      let z_total0 =
        value.quantity_of(input_value, asset_z_policy, asset_z_name)
      let x_total1 =
        value.quantity_of(output_value, asset_x_policy, asset_x_name)
      let y_total1 =
        value.quantity_of(output_value, asset_y_policy, asset_y_name)
      let z_total1 =
        value.quantity_of(output_value, asset_z_policy, asset_z_name)
      let x_total_delta = x_total1 - x_total0
      let y_total_delta = y_total1 - y_total0
      let z_total_delta = z_total1 - z_total0

      let protocol_fees_x_delta = protocol_fees_x1 - protocol_fees_x0
      let protocol_fees_y_delta = protocol_fees_y1 - protocol_fees_y0
      let protocol_fees_z_delta = protocol_fees_z1 - protocol_fees_z0

      let valid_fees_amount_withdrawned: Bool =
        x_total_delta == protocol_fees_x_delta && y_total_delta == protocol_fees_y_delta && z_total_delta == protocol_fees_z_delta

      let params_preserved_for_protocol_fees_withdrawn: Bool =
        input_stake_cred == output_stake_cred && an2n0 == an2n1 && lp_fee_num1 == lp_fee_num0 && protocol_fee_num1 == protocol_fee_num0 && treasury_addr1 == treasury_addr0
      params_preserved_for_protocol_fees_withdrawn && valid_fees_amount_withdrawned && protocol_fees_x1 >= 0 && protocol_fees_y1 >= 0 && protocol_fees_z1 >= 0
    } else if
    dao_action_ix == 4{
    
      // Update 'ampl_coeff' value of the pool.
      // NB: 'ampl_coeff' can be updated only for balanced pool,
      // where invariant value doesn't depend on 'an2n', the 'invatiant_preserved' flag guarantees this.
      let ampl_coeff1 = an2n1 / n2n

      let ampl_coeff_inside_bounds: Bool =
        ampl_coeff1 >= ampl_coeff_min && ampl_coeff1 <= ampl_coeff_max
      let params_preserved_for_ampl_coeff_update: Bool =
        protocol_fees_x0 == protocol_fees_x1 && protocol_fees_y0 == protocol_fees_y1 && protocol_fees_z0 == protocol_fees_z1 && input_value == output_value && input_stake_cred == output_stake_cred && lp_fee_num1 == lp_fee_num0 && protocol_fee_num1 == protocol_fee_num0 && treasury_addr1 == treasury_addr0
      ampl_coeff_is_editable && ampl_coeff_inside_bounds && params_preserved_for_ampl_coeff_update
    } else if
    dao_action_ix == 5{
    
      // Update staking credential of the pool:
      let params_preserved_for_staking_credential_update: Bool =
        protocol_fees_x0 == protocol_fees_x1 && protocol_fees_y0 == protocol_fees_y1 && protocol_fees_z0 == protocol_fees_z1 && input_value == output_value && an2n0 == an2n1 && lp_fee_num1 == lp_fee_num0 && protocol_fee_num1 == protocol_fee_num0 && treasury_addr1 == treasury_addr0
      params_preserved_for_staking_credential_update
    } else {
      False
    }
  lp_token_preserved && valid_dao_action
}
