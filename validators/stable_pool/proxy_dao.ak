use aiken/list
use aiken/transaction.{
  InlineDatum, Input, Output, ScriptContext, Spend, Transaction,
}
use aiken/transaction/value.{Value}
use splash/plutus.{Asset}
use splash/stableswap/types.{DAOAction, DAODatum, PoolData}
use splash/stableswap/utils

// Limits of the adjustable parameters:

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
      datum: InlineDatum(pool_in_datum),
      ..
    } = pool_input.output
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
      datum: InlineDatum(pool_out_datum),
      ..
    } = pool_out_output
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
        input_datum,
        pool_out_value,
        output_datum,
        dao_action_ix,
      )

    valid_dao_input && proxy_dao_credentials_preserved && proxy_dao_congif_preserved && no_more_tokens_in_proxy_dao_out && valid_dao_action
  }
}

fn validate_stable_pool_dao_action(
  input_value: Value,
  input_datum: PoolData,
  output_value: Value,
  output_datum: PoolData,
  dao_action_ix: Int,
) -> Bool {
  // ==================================== INPUT DATA ==================================== //
  // Pool adjustable params| Input:
  let PoolData {
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
  // Predicate about protocol fees (they must be preserved in all actions except "withdrawn protocol fees"):
  let protocol_fees_preserved: Bool =
    protocol_fees0 == protocol_fees1 && input_value == output_value

  // 3. Action is valid:
  let valid_dao_action =
    if dao_action_ix == 0 {
      // Change liquidity provider fee:
      let swap_fee_inside_bounds: Bool =
        lp_fee_num1 >= lp_fee_num_min && lp_fee_num1 <= lp_fee_num_max
      let non_swap_fee_params_preserved: Bool =
        protocol_fee_num0 == protocol_fee_num1 && dao_stabe_proxy_witness0 == dao_stabe_proxy_witness1 && treasury_addr0 == treasury_addr1

      swap_fee_inside_bounds && non_swap_fee_params_preserved && protocol_fees_preserved
    } else if
    dao_action_ix == 1{
    
      // Change protocol fee num:
      let protocol_fee_num_inside_bounds: Bool =
        protocol_fee_num1 >= protocol_fee_num_min && protocol_fee_num1 <= protocol_fee_num_max

      let params_preserved_for_protocol_fee_num_change: Bool =
        lp_fee_num1 == lp_fee_num0 && treasury_addr1 == treasury_addr0 && dao_stabe_proxy_witness1 == dao_stabe_proxy_witness0
      protocol_fee_num_inside_bounds && params_preserved_for_protocol_fee_num_change && protocol_fees_preserved
    } else if
    dao_action_ix == 2{
    
      // Change treasury address:
      let params_preserved_for_treasury_addr_change: Bool =
        lp_fee_num1 == lp_fee_num0 && protocol_fee_num1 == protocol_fee_num0 && dao_stabe_proxy_witness1 == dao_stabe_proxy_witness0
      params_preserved_for_treasury_addr_change && protocol_fees_preserved
    } else if dao_action_ix == 3 {
      // Withdrawn protocol fees:
      let total_balances0 =
        utils.extract_target_balances(input_datum.tradable_assets, input_value)
      let total_balances1 =
        utils.extract_target_balances(input_datum.tradable_assets, output_value)
      let total_reservas_deltas =
        utils.lists_diff(total_balances1, total_balances0)

      let collected_protocol_fees_deltas =
        utils.lists_diff(protocol_fees1, protocol_fees0)
      let valid_fees_amount_withdrawned: Bool =
        total_reservas_deltas == collected_protocol_fees_deltas

      let params_preserved: Bool =
        lp_fee_num1 == lp_fee_num0 && protocol_fee_num1 == protocol_fee_num0 && treasury_addr1 == treasury_addr0 && dao_stabe_proxy_witness1 == dao_stabe_proxy_witness0
      params_preserved && valid_fees_amount_withdrawned
    } else if dao_action_ix == 4 {
      // Update DAO-proxy witness:
      let params_preserved_for_dao_witness_change: Bool =
        lp_fee_num1 == lp_fee_num0 && protocol_fee_num1 == protocol_fee_num0 && treasury_addr1 == treasury_addr0
      params_preserved_for_dao_witness_change && protocol_fees_preserved
    } else {
      False
    }
  invatiant_preserved && lp_token_preserved && valid_dao_action
}
