use splash/plutus.{Asset, ScriptHash, VerificationKeyHash}

// ==================================== POOL ==================================== //

/// Pool data T2T.
pub type PoolDataT2T {
  // Identifier of the pool | Immutable:
  pool_nft: Asset,
  // StableSwap invariant amplification coefficient multiplied by 'n ^ (2n)',
  // where 'n' is the number of tradable assets | Mutable if 'ampl_coeff_is_editable' is True:
  an2n: Int,
  // Identifiers of the tradable assets | Immutable:
  asset_x: Asset,
  asset_y: Asset,
  // Precision multipliers for calculations, i.e. precision / decimals.
  // Precision must be fixed as a maximum value of tradable assets decimals | Immutable:
  multiplier_x: Int,
  multiplier_y: Int,
  // Identifier of the liquidity token, representing user's share in the pool | Immutable:
  lp_token: Asset,
  // Flag if amplification coefficient is editable | Immutable:
  ampl_coeff_is_editable: Bool,
  // Flag if liquidity provider fee is editable | Immutable:
  lp_fee_is_editable: Bool,
  // Numerator of the liquidity provider fee | Mutable if 'lp_fee_is_editable' is True:
  lp_fee_num: Int,
  // Numerator of the protocol fee share | Mutable:
  protocol_fee_num: Int,
  // Information about the DAO script, which audits the correctness of the "DAO-actions" with stable pool | Immutable:
  dao_stabe_proxy_witness: ScriptHash,
  // Treasury address | Mutable:
  treasury_address: ScriptHash,
  // Collected (and currently available) protocol fees in the tradable assets native units | Mutable:
  protocol_fees_x: Int,
  protocol_fees_y: Int,
}

/// Pool data T2T2T.
pub type PoolDataT2T2T {
  // Identifier of the pool | Immutable:
  pool_nft: Asset,
  // StableSwap invariant amplification coefficient multiplied by 'n ^ (2n)',
  // where 'n' is the number of tradable assets | Mutable if 'ampl_coeff_is_editable' is True:
  an2n: Int,
  // Identifiers of the tradable assets | Immutable:
  asset_x: Asset,
  asset_y: Asset,
  asset_z: Asset,
  // Precision multipliers for calculations, i.e. precision / decimals.
  // Precision must be fixed as a maximum value of tradable assets decimals | Immutable:
  multiplier_x: Int,
  multiplier_y: Int,
  multiplier_z: Int,
  // Identifier of the liquidity token, representing user's share in the pool | Immutable:
  lp_token: Asset,
  // Flag if amplification coefficient is editable | Immutable:
  ampl_coeff_is_editable: Bool,
  // Flag if liquidity provider fee is editable | Immutable:
  lp_fee_is_editable: Bool,
  // Numerator of the liquidity provider fee | Mutable if 'lp_fee_is_editable' is True:
  lp_fee_num: Int,
  // Numerator of the protocol fee share | Mutable:
  protocol_fee_num: Int,
  // Information about the DAO script, which audits the correctness of the "DAO-actions" with stable pool | Mutable:
  dao_stabe_proxy_witness: ScriptHash,
  // Treasury address | Mutable:
  treasury_address: ScriptHash,
  // Collected (and currently available) protocol fees in the tradable assets native units | Mutable:
  protocol_fees_x: Int,
  protocol_fees_y: Int,
  protocol_fees_z: Int,
}

/// Pool action types.
pub type PoolAction {
  AMMAction { context_values_list: List<Int> }
  // "context_values_list" for swap actions contains:
  // - 0: Value of the StableSwap invariant for the output state;
  PDAOAction
}

pub type PoolRedeemer {
  pool_in_ix: Int,
  pool_out_ix: Int,
  action: PoolAction,
}

// ==================================== POOL AMM-ORDERS ==================================== //

/// AMM-orders data.
pub type DepositData {
  // Identifier of the pool:
  pool_nft: Asset,
  // Redeemer PK:
  redeemer_pk: VerificationKeyHash,
  // Redeemer stake credentials:
  redeemer_sc: VerificationKeyHash,
  // Minimum expected amount of liquidity tokens:
  min_expected_lp_amount: Int,
  // Execution fee:
  ex_fee: Int,
}

pub type RedeemDataT2T {
  // Identifier of the pool:
  pool_nft: Asset,
  // Redeemer PK:
  redeemer_pk: VerificationKeyHash,
  // Redeemer stake credentials:
  redeemer_sc: VerificationKeyHash,
  // Minimum expected balances of tradable assets:
  min_expected_x: Int,
  min_expected_y: Int,
  // Execution fee:
  ex_fee: Int,
}

pub type RedeemDataT2T2T {
  // Identifier of the pool:
  pool_nft: Asset,
  // Redeemer PK:
  redeemer_pk: VerificationKeyHash,
  // Redeemer stake credentials:
  redeemer_sc: VerificationKeyHash,
  // Minimum expected balances of tradable assets:
  min_expected_x: Int,
  min_expected_y: Int,
  min_expected_z: Int,
  // Execution fee:
  ex_fee: Int,
}

/// Order action types.
pub type OrderAction {
  ApplyOrder { redeemer_in_ix: Int, redeemer_out_ix: Int, pool_in_ix: Int }
  CancelOrder
}

// ==================================== DAO ==================================== //

/// DAO contract config (congig is immutable).
pub type DAOData {
  // Identifier of the pool:
  pool_nft: Asset,
}

/// DAO action types:
pub type DAOAction {
  pool_in_ix: Int,
  pool_out_ix: Int,
  dao_in_ix: Int,
  dao_out_ix: Int,
  // Value of the 'dao_action_ix' is one from the set:
  // - 0: Update 'lp_fee_num';
  // - 1: Update 'protocol_fee_num';
  // - 2: Update 'treasury_address';
  // - 3: Withdrawn protocol fees;
  // - 4: Update StableSwap invariant amplification coefficient ('an2n');
  // - 5: Update staking credential of the pool.
  dao_action_ix: Int,
}
