use splash/plutus.{Asset, ScriptHash, VerificationKeyHash}

// ==================================== POOL ==================================== //

/// Pool data.
pub type PoolData {
  // Identifier of the pool | Immutable:
  pool_nft: Asset,
  // Number of tradable assets in the pool | Immutable:
  n: Int,
  // Identifiers of the tradable assets | Immutable:
  tradable_assets: List<Asset>,
  // Precision multipliers for calculations, i.e. precision / decimals.
  // Precision must be fixed as maximum value of tradable tokens decimals | Immutable:
  tradable_tokens_multipliers: List<Int>,
  // Identifier of the liquidity token, representing user's share in the pool | Immutable:
  lp_token: Asset,
  // Flag if liquidity provider fee is editable | Immutable:
  lp_fee_is_editable: Bool,
  // Invariant's amplification coefficient | Mutable:
  ampl_coeff: Int,
  // Numerator of the liquidity provider fee | Mutable:
  lp_fee_num: Int,
  // Numerator of the protocol fee share | Mutable:
  protocol_fee_num: Int,
  // Information about the DAO script, which audits the correctness of the "DAO-actions" with stable pool | Mutable:
  dao_stabe_proxy_witness: List<ScriptHash>,
  // Treasury address | Mutable:
  treasury_address: ScriptHash,
  // Collected (and currently available) protocol fees in the tradable assets native units | Mutable:
  protocol_fees: List<Int>,
  // Actual value of the pool's invariant | Mutable:
  inv: Int,
}

/// Pool action types.
pub type PoolAction {
  AMMAction {
    // Value of the 'option_int0' is index of base asset for swap and native invariant value for deposit/redeem.
    option_int0: Int,
    // Value of the 'option_int0' is index of quote asset for swap and invariant with LP fees value for deposit/redeem.
    option_int1: Int,
  }
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
  redeemer: VerificationKeyHash,
  // Minimum expected amount of liquidity tokens:
  min_expected_lp_amount: Int,
}

pub type RedeemUniformData {
  // Identifier of the pool:
  pool_nft: Asset,
  // Redeemer PK:
  redeemer: VerificationKeyHash,
  // Minimum expected balances of tradable tokens:
  min_expected_received_assets_balances: List<Int>,
}

pub type RedeemData {
  // Identifier of the pool:
  pool_nft: Asset,
  // Redeemer PK:
  redeemer: VerificationKeyHash,
  // Expected assets:
  expected_assets: List<Asset>,
  // Minimum expected balances of tradable tokens:
  min_expected_received_assets_balances: List<Int>,
  // Minimum expected balances of LP tokens:
  min_expected_lp_change: Int,
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
  // - 0: Update liquidity provider num;
  // - 1: Update protocol fee num;
  // - 2: Update treasury address;
  // - 3: Update DAO-proxy witness;
  // - 4: Withdrawn protocol fees;
  // - 5: Update 'ampl_coeff' value;
  // - 6: Update staking credential of the pool.
  dao_action_ix: Int,
}
