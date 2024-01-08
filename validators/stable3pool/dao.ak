use aiken/list
use aiken/transaction.{
  Input, Output, ScriptContext, Spend, Transaction, find_input,
}
use aiken/transaction/value.{quantity_of}
use whalepools/stable3pool/types.{DAOAction, PoolData}
use whalepools/stable3pool/utils.{convert_dao_datum, convert_pool_datum}

// Limits of the adjustable parameters:
const ampl_coeff_min = 1

const ampl_coeff_max = 100_000

const swap_fee_num_min = 0

const swap_fee_num_max = 6_999

const protocol_share_num_min = 1

const protocol_share_num_max = 999

// DAO stable pool validator:
validator {
  fn stable_pool_dao(action: DAOAction, ctx: ScriptContext) -> Bool {
    let ScriptContext { transaction, purpose } = ctx
    expect Spend(spent_utxo_reference) = purpose
    // ==================================== INPUT ==================================== //
    expect Some(self_input) =
      find_input(transaction.inputs, spent_utxo_reference)
    let self = self_input.output
    let dao_datum0 = convert_dao_datum(self.datum)
    let verified_keys = dao_datum0.admin_pkhs
    let threshold = dao_datum0.threshold

    expect Some(pool_input) = list.at(transaction.inputs, action.pool_in_ix)
    let pool_in = pool_input.output
    let pool_datum_old = convert_pool_datum(pool_in.datum)

    let script0 = self.reference_script
    // Pool's tokens | Input:
    let pool_nft0 = pool_datum_old.pool_nft
    let base_token0 = pool_datum_old.base_token
    let quote0_token0 = pool_datum_old.quote0_token
    let quote1_token0 = pool_datum_old.quote1_token
    let lp_token0 = pool_datum_old.lp_token

    // Decimal values ​​of tradable assets | Input:
    let base_dec0 = pool_datum_old.base_dec
    let quote0_dec0 = pool_datum_old.quote0_dec
    let quote1_dec0 = pool_datum_old.quote1_dec

    // Treasury & DAO | Input:
    let treasury_addr0 = pool_datum_old.treasury_address
    let dao_policy0 = pool_datum_old.dao_policy

    // Actual balance of collected protocol fees (accumulated in the pool) | Input:
    let base_protocol_fees0 = pool_datum_old.base_protocol_fees
    let quote0_protocol_fees0 = pool_datum_old.quote0_protocol_fees
    let quote1_protocol_fees0 = pool_datum_old.quote1_protocol_fees

    // Amplification coefficient of the StableSwap invariant | Input:
    let a0 = pool_datum_old.ampl_coeff

    // Numerators of fees | Input:
    let swap_fee_num0 = pool_datum_old.swap_fee_num
    let protocol_share_num0 = pool_datum_old.protocol_share_num

    // Values of the invariant | Input:
    let inv_native0 = pool_datum_old.inv_native
    let inv0 = pool_datum_old.inv

    // ==================================== OUTPUT ==================================== //
    expect Some(successor) = list.at(transaction.outputs, action.dao_out_ix)
    let dao_datum1 = convert_dao_datum(successor.datum)

    expect Some(pool_out) = list.at(transaction.outputs, action.pool_out_ix)
    let pool_datum_new = convert_pool_datum(pool_out.datum)
    let script1 = pool_out.reference_script

    // Pool's tokens | Output:
    let pool_nft1 = pool_datum_new.pool_nft
    let base_token1 = pool_datum_new.base_token
    let quote0_token1 = pool_datum_new.quote0_token
    let quote1_token1 = pool_datum_new.quote1_token
    let lp_token1 = pool_datum_new.lp_token

    // Decimal values ​​of tradable assets | Output:
    let base_dec1 = pool_datum_new.base_dec
    let quote0_dec1 = pool_datum_new.quote0_dec
    let quote1_dec1 = pool_datum_new.quote1_dec

    // Treasury & DAO | Output:
    let treasury_addr1 = pool_datum_new.treasury_address
    let dao_policy1 = pool_datum_new.dao_policy

    // Actual balance of collected protocol fees (accumulated in the pool) | Output:
    let base_protocol_fees1 = pool_datum_new.base_protocol_fees
    let quote0_protocol_fees1 = pool_datum_new.quote0_protocol_fees
    let quote1_protocol_fees1 = pool_datum_new.quote1_protocol_fees

    // Amplification coefficient of the StableSwap invariant | Output:
    let a1 = pool_datum_new.ampl_coeff

    // Numerators of fees | Output:
    let swap_fee_num1 = pool_datum_new.swap_fee_num
    let protocol_share_num1 = pool_datum_new.protocol_share_num

    // Values of the invariant | Output:
    let inv_native1 = pool_datum_new.inv_native
    let inv1 = pool_datum_new.inv
    // ==================================== MAIN VALIDATIONS ==================================== //
    // DAO datum is preserved:
    let dao_datum_preserved = dao_datum1 == dao_datum0
    // Check if tx is signed by the majority:
    let approved_tx =
      list.count(
        verified_keys,
        fn(pk) { list.has(ctx.transaction.extra_signatories, pk) },
      ) >= threshold
    // Check if immutable for non-AMM actions parameters are preserved:
    let inv_values_preserved = inv1 == inv0 && inv_native1 == inv_native0
    let pool_identifiers_preserved =
      pool_nft1 == pool_nft0 && script1 == script0
    let pool_token_ids_preserved =
      base_token1 == base_token0 && quote0_token1 == quote0_token0 && quote1_token1 == quote1_token0 && lp_token1 == lp_token0
    let pool_token_dec_preserved =
      base_dec1 == base_dec0 && quote0_dec1 == quote0_dec0 && quote1_dec1 == quote1_dec0
    let immutable_params_preserved =
      inv_values_preserved && pool_identifiers_preserved && pool_token_ids_preserved && pool_token_dec_preserved
    // ==================================== ACTION VALIDATIONS ==================================== //
    // Pool's tokens policies:
    let base_policy = base_token0.policy
    let quote0_policy = quote0_token0.policy
    let quote1_policy = quote1_token0.policy
    let lp_policy = lp_token0.policy

    let base_amount_total0 =
      quantity_of(self.value, base_policy, base_token0.name)

    let base_amount_total1 =
      quantity_of(successor.value, base_policy, base_token0.name)

    let quote0_amount_total0 =
      quantity_of(self.value, quote0_policy, quote0_token0.name)

    let quote0_amount_total1 =
      quantity_of(successor.value, quote0_policy, quote0_token0.name)

    let quote1_amount_total0 =
      quantity_of(self.value, quote1_policy, quote1_token0.name)

    let quote1_amount_total1 =
      quantity_of(successor.value, quote1_policy, quote1_token1.name)

    let lp_amount0 = quantity_of(self.value, lp_policy, lp_token0.name)
    let lp_amount1 = quantity_of(successor.value, lp_policy, lp_token0.name)

    let protocol_fees_preserved =
      base_protocol_fees1 == base_protocol_fees0 && quote0_protocol_fees1 == quote0_protocol_fees0 && quote1_protocol_fees1 == quote1_protocol_fees0
    let reserves_preserved =
      base_amount_total1 == base_amount_total0 && quote0_amount_total1 == quote0_amount_total0 && quote1_amount_total1 == quote1_amount_total0 && lp_amount1 == lp_amount0
    let not_a_withdrawal = protocol_fees_preserved && reserves_preserved
    let valid_action =
      // Change swap fee:
      if action.action_ix == 0 {
        let swap_fee_inside_bounds =
          swap_fee_num1 >= swap_fee_num_min && swap_fee_num1 <= swap_fee_num_max
        let params_preserved_for_swap_fee_change =
          protocol_share_num1 == protocol_share_num0 && a1 == a0 && treasury_addr1 == treasury_addr0 && dao_policy1 == dao_policy0
        not_a_withdrawal && swap_fee_inside_bounds && params_preserved_for_swap_fee_change
      } else if
      // Change protocol fee share:
      action.action_ix == 1{
      
        let protocol_share_num_inside_bounds =
          protocol_share_num1 >= protocol_share_num_min && protocol_share_num1 <= protocol_share_num_max

        let params_preserved_for_protocol_share_num_change =
          swap_fee_num1 == swap_fee_num0 && a1 == a0 && treasury_addr1 == treasury_addr0 && dao_policy1 == dao_policy0
        not_a_withdrawal && protocol_share_num_inside_bounds && params_preserved_for_protocol_share_num_change
      } else if
      // Change StableSwap amplification coefficient:
      action.action_ix == 2{
      
        let ampl_coeff_inside_bounds =
          a1 >= ampl_coeff_min && a1 <= ampl_coeff_max
        let params_preserved_for_ampl_coeff_change =
          swap_fee_num1 == swap_fee_num0 && protocol_share_num1 == protocol_share_num0 && treasury_addr1 == treasury_addr0 && dao_policy1 == dao_policy0
        not_a_withdrawal && ampl_coeff_inside_bounds && params_preserved_for_ampl_coeff_change
      } else if
      // Withdraw protocol treasury:
      action.action_ix == 3{
      
        // Check the amounts:
        expect Some(treasury_successor) =
          list.at(transaction.outputs, action.treasury_out_ix)

        let base_amount_treasury =
          quantity_of(treasury_successor.value, base_policy, base_token0.name)

        let quote0_amount_treasury =
          quantity_of(
            treasury_successor.value,
            quote0_policy,
            quote0_token0.name,
          )

        let quote1_amount_treasury =
          quantity_of(
            treasury_successor.value,
            quote1_policy,
            quote1_token0.name,
          )

        let valid_withdraw_deltas =
          base_amount_treasury == base_amount_total0 - base_amount_total1 && quote0_amount_treasury == quote0_amount_total0 - quote0_amount_total1 && quote1_amount_treasury == quote1_amount_total0 - quote1_amount_total1
        let valid_protocol_fees =
          base_protocol_fees1 == base_protocol_fees0 - base_amount_treasury && quote0_protocol_fees1 == quote0_protocol_fees0 - quote0_amount_treasury && quote1_protocol_fees1 == quote1_protocol_fees0 - quote1_amount_treasury
        let valid_treasury = True
        // TODO
        let params_preserved_for_withdraw_treasury =
          swap_fee_num1 == swap_fee_num0 && protocol_share_num1 == protocol_share_num0 && treasury_addr1 == treasury_addr0 && dao_policy1 == dao_policy0
        valid_treasury && valid_withdraw_deltas && valid_protocol_fees && params_preserved_for_withdraw_treasury && lp_amount1 == lp_amount0
      } else if
      // Change treasury address:
      action.action_ix == 4{
      
        let params_preserved_for_treasury_addr_change =
          swap_fee_num1 == swap_fee_num0 && protocol_share_num1 == protocol_share_num0 && a1 == a0 && dao_policy1 == dao_policy0
        not_a_withdrawal && params_preserved_for_treasury_addr_change
      } else if // Change admin address:
      action.action_ix == 5 {
        let params_preserved_for_dao_policy_change =
          swap_fee_num1 == swap_fee_num0 && protocol_share_num1 == protocol_share_num0 && treasury_addr1 == treasury_addr0
        not_a_withdrawal && params_preserved_for_dao_policy_change
      } else {
        False
      }
    dao_datum_preserved && approved_tx && immutable_params_preserved && valid_action
  }
}
