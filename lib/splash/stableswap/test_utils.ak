use aiken/bytearray
use aiken/int
use aiken/list
use aiken/string
use aiken/transaction.{
  InlineDatum, Input, Output, OutputReference, ScriptContext, Spend, Transaction,
  TransactionId,
}
use aiken/transaction/credential.{from_script, with_delegation_script}
use aiken/transaction/value.{Value}
use splash/plutus.{Asset}
use splash/stableswap/types.{DAOData, PoolData}

const default_lovelace_value = 1_000_000_000_000_000

pub const lp_emission = 340_282_366_920_938_463_463_374_607_431_768_211_455

// Generate policies:
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

/// Returns PoolData and Value with a given params for t2t pool:
pub fn t2t_pool_state(
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
      lp_fee_is_editable: True,
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

/// Returns PoolData and Value with a given params for n2t pool:
pub fn n2t_pool_state(
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
      lp_fee_is_editable: True,
      ampl_coeff,
      lp_fee_num,
      protocol_fee_num,
      dao_stabe_proxy_witness: [bytearray.from_string(dao_policy_string)],
      treasury_address: bytearray.from_string(treasury_address_string),
      protocol_fees,
      inv,
    }

  expect Some(asset1) = list.at(tradable_assets, 1)
  expect Some(balance0) = list.at(tradable_balances, 0)
  expect Some(balance1) = list.at(tradable_balances, 1)

  let min_value =
    value.from_lovelace(balance0)
      |> value.add(pool_data.pool_nft.policy, pool_data.pool_nft.name, 1)
      |> value.add(pool_data.lp_token.policy, pool_data.lp_token.name, lp_balance)
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

/// Returns DAOData and Value with a given params:
pub fn proxy_dao_state(
  pool_nft_policy_string: String,
  pool_nft_name_string: String,
) -> (DAOData, Value) {
  let proxy_dao_data =
    DAOData {
      pool_nft: Asset {
        policy: bytearray.from_string(pool_nft_policy_string),
        name: bytearray.from_string(pool_nft_name_string),
      },
    }

  let input_value = value.from_lovelace(default_lovelace_value)
  (proxy_dao_data, input_value)
}

/// Returns pool state transition context:
pub fn make_pool_state_transition_context(
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

/// Returns proxy-DAO state transition context:
pub fn make_proxy_dao_tx_context(
  input_proxy_dao_data: DAOData,
  input_proxy_dao_value: Value,
  output_proxy_dao_data: DAOData,
  output_proxy_dao_value: Value,
  input_pool_data: PoolData,
  input_pool_value: Value,
  output_pool_data: PoolData,
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
  let proxy_dao_ref_script =
    Some(#"f2c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50")
  let placeholder_utxo =
    OutputReference { transaction_id: TransactionId(""), output_index: 1 }
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

  let proxy_dao_input_utxo =
    Output {
      address: proxy_dao_addr,
      value: input_proxy_dao_value,
      datum: InlineDatum(input_proxy_dao_data),
      reference_script: proxy_dao_ref_script,
    }

  let proxy_dao_output_utxo =
    Output {
      address: proxy_dao_addr,
      value: output_proxy_dao_value,
      datum: InlineDatum(output_proxy_dao_data),
      reference_script: proxy_dao_ref_script,
    }

  let proxy_dao_input =
    Input { output_reference: placeholder_utxo, output: proxy_dao_input_utxo }

  ScriptContext {
    purpose: Spend(placeholder_utxo),
    transaction: transaction.placeholder()
      |> fn(transaction) {
           Transaction {
             ..transaction,
             inputs: [pool_input, proxy_dao_input],
             outputs: [pool_output_utxo, proxy_dao_output_utxo],
           }
         },
  }
}

/// Returns proxy-DAO state transition context for update pool staking cred. action:
pub fn make_proxy_dao_tx_context_with_new_pool_stake_cred(
  input_proxy_dao_data: DAOData,
  input_proxy_dao_value: Value,
  output_proxy_dao_data: DAOData,
  output_proxy_dao_value: Value,
  input_pool_data: PoolData,
  input_pool_value: Value,
  output_pool_data: PoolData,
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
  let proxy_dao_ref_script =
    Some(#"f2c9f9c4252d86702c2f4c2e49e6648c7cffe3c8f2b6b7d779788f50")
  let placeholder_utxo =
    OutputReference { transaction_id: TransactionId(""), output_index: 1 }
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

  let proxy_dao_input_utxo =
    Output {
      address: proxy_dao_addr,
      value: input_proxy_dao_value,
      datum: InlineDatum(input_proxy_dao_data),
      reference_script: proxy_dao_ref_script,
    }

  let proxy_dao_output_utxo =
    Output {
      address: proxy_dao_addr,
      value: output_proxy_dao_value,
      datum: InlineDatum(output_proxy_dao_data),
      reference_script: proxy_dao_ref_script,
    }

  let proxy_dao_input =
    Input { output_reference: placeholder_utxo, output: proxy_dao_input_utxo }

  ScriptContext {
    purpose: Spend(placeholder_utxo),
    transaction: transaction.placeholder()
      |> fn(transaction) {
           Transaction {
             ..transaction,
             inputs: [pool_input, proxy_dao_input],
             outputs: [pool_output_utxo, proxy_dao_output_utxo],
           }
         },
  }
}
