use aiken/cbor.{serialise}
use aiken/hash.{Blake2b_256, Hash, blake2b_256}
use aiken/transaction.{
  InlineDatum, Input, Output, OutputReference, ScriptContext, Spend, Transaction,
  TransactionId,
}
use aiken/transaction/credential.{from_script}
use aiken/transaction/value.{Value, add, from_lovelace}
use whalepools/plutus.{Asset}
use whalepools/stable3pool/types.{PoolData}

pub type PoolTestAction {
  /// Change pool state.
  PoolAction {
    input_data: PoolData,
    successor_ix: Int,
    successor_data_hash: Hash<Blake2b_256, Data>,
  }
}

// Default template policies:
const pool_nft_policy =
  #"0000000000000000000000000000000000000000000000000000000000000001"

const pool_nft_name =
  #"1000000000000000000000000000000000000000000000000000000000000000"

const base_token_policy =
  #"0000000000000000000000000000000000000000000000000000000000000002"

const base_token_name =
  #"2000000000000000000000000000000000000000000000000000000000000000"

const quote0_token_policy =
  #"0000000000000000000000000000000000000000000000000000000000000003"

const quote0_token_name =
  #"3000000000000000000000000000000000000000000000000000000000000000"

const quote1_token_policy =
  #"0000000000000000000000000000000000000000000000000000000000000004"

const quote1_token_name =
  #"4000000000000000000000000000000000000000000000000000000000000000"

const lp_token_policy =
  #"0000000000000000000000000000000000000000000000000000000000000005"

const lp_token_name =
  #"5000000000000000000000000000000000000000000000000000000000000000"

const default_lovelace_value = 1_000_000_000_000

pub fn pool_data_with_default_config(
  base_protocol_fees: Int,
  quote0_protocol_fees: Int,
  quote1_protocol_fees: Int,
  inv_native: Int,
  inv: Int,
) -> PoolData {
  let dao_policy_list =
    [#"0000000000000000000000000000000000000000000000000000000000000006"]

  let treasury_address_init =
    #"afddc16c18e7d8de379fb9aad39b3d1b5afd27603e5ebac818432a72"
  PoolData {
    pool_nft: Asset { policy: pool_nft_policy, name: pool_nft_name },
    base_token: Asset { policy: base_token_policy, name: base_token_name },
    quote0_token: Asset { policy: quote0_token_policy, name: quote0_token_name },
    quote1_token: Asset { policy: quote1_token_policy, name: quote1_token_name },
    lp_token: Asset { policy: lp_token_policy, name: lp_token_name },
    base_dec: 1_000,
    quote0_dec: 1_000_000,
    quote1_dec: 1_000,
    ampl_coeff: 300,
    swap_fee_num: 200,
    dao_policy: dao_policy_list,
    treasury_address: treasury_address_init,
    protocol_share_num: 500,
    base_protocol_fees,
    quote0_protocol_fees,
    quote1_protocol_fees,
    inv_native,
    inv,
  }
}

pub fn pool_value(
  base_amount: Int,
  quote0_amount: Int,
  quote1_amount: Int,
  lp_amount: Int,
) -> Value {
  let input_value = from_lovelace(default_lovelace_value)
  let input_value =
    add(
      input_value,
      policy_id: pool_nft_policy,
      asset_name: pool_nft_name,
      quantity: 1,
    )
  let input_value =
    add(
      input_value,
      policy_id: base_token_policy,
      asset_name: base_token_name,
      quantity: base_amount,
    )
  let input_value =
    add(
      input_value,
      policy_id: quote0_token_policy,
      asset_name: quote0_token_name,
      quantity: quote0_amount,
    )
  let input_value =
    add(
      input_value,
      policy_id: quote1_token_policy,
      asset_name: quote1_token_name,
      quantity: quote1_amount,
    )
  add(
    input_value,
    policy_id: lp_token_policy,
    asset_name: lp_token_name,
    quantity: lp_amount,
  )
}

pub fn pool_amm_action_state_transition_tx(
  input_data: PoolData,
  input_value: Value,
  output_data: PoolData,
  output_value: Value,
) -> (ScriptContext, PoolTestAction) {
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
  let output_datum_hash = blake2b_256(serialise(output_data))

  (
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
    },
    PoolAction {
      input_data,
      successor_ix: 0,
      successor_data_hash: output_datum_hash,
    },
  )
}
