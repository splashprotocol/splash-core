use aiken/transaction/value.{PolicyId}
use whalepools/plutus.{Asset, ScriptKeyHash, VerificationKeyHash}

pub type PoolData {
  /// Stable3Pool datum.
  PoolData {
    // Identifier of the pool:
    pool_nft: Asset,
    // Identifier of the base asset:
    base_token: Asset,
    // Identifier of the first quote asset:
    quote0_token: Asset,
    // Identifier of the second quote asset:
    quote1_token: Asset,
    // Identifier of the liquidity token asset:
    lp_token: Asset,
    // Decimals of the base asset:
    base_dec: Int,
    // Decimals of the first quote asset:
    quote0_dec: Int,
    // Decimals of the second quote asset:
    quote1_dec: Int,
    // Invariant's amplification coefficient:
    ampl_coeff: Int,
    // Numerator of the swap fee:
    swap_fee_num: Int,
    // Numerator of the protocol fee share:
    protocol_share_num: Int,
    // Information about the DAO policy, which audits the correctness of the following actions: "Withdraw treasury", "Change swap fee", "Change protocol share" and "Change treasury address":
    dao_policy: List<PolicyId>,
    // Treasury address:
    treasury_address: ScriptKeyHash,
    // Collected protocol fees in base asset units:
    base_protocol_fees: Int,
    // Collected protocol fees in first quote asset units:
    quote0_protocol_fees: Int,
    // Collected protocol fees in second quote asset units:
    quote1_protocol_fees: Int,
    // Value of the pool's invariant before fees are applied to the last AMM operation:
    inv_native: Int,
    // Actual value of the pool's invariant:
    inv: Int,
  }
}

pub type DepositData {
  /// Deposit order datum.
  DepositData {
    // Identifier of the pool:
    pool_nft: Asset,
    // Redeemer PK:
    redeemer: VerificationKeyHash,
    // Minimum expected amount of liquidity tokens:
    min_expected_lp_amount: Int,
  }
}

pub type RedeemData {
  /// Redeem order datum.
  RedeemData {
    // Identifier of the pool:
    pool_nft: Asset,
    // Redeemer PK:
    redeemer: VerificationKeyHash,
    // Minimum expected amount of base tokens:
    min_expected_base_amount: Int,
    // Minimum expected amount of quote0 tokens:
    min_expected_quote0_amount: Int,
    // Minimum expected amount of quote1 tokens:
    min_expected_quote1_amount: Int,
  }
}

pub type DAOData {
  /// DAO contract datum.
  DAOData {
    // List of authorized keys, which are allowed to perform actions:
    admin_pkhs: List<VerificationKeyHash>,
    // Identifier of the pool:
    pool_nft: Asset,
    // The minimum number of signatures that must be present
    // in the TX to verify the validity of any non-AMM action:
    threshold: Int,
  }
}

pub type PoolAction {
  /// Change pool state.
  PoolAction { successor_ix: Int }
}

pub type DepositAction {
  /// Execute deposit order.
  ExecDeposit { pool_in_ix: Int, successor_ix: Int }
  CancelDeposit
}

pub type RedeemAction {
  /// Execute redeem order.
  ExecRedeem { pool_in_ix: Int, successor_ix: Int }
  CancelRedeem
}

pub type DAOAction {
  // Value of 'action_ix' is one from the set:
  // - 0: Change swap fee
  // - 1: Change protocol fee share
  // - 2: Change StableSwap amplification coefficient
  // - 3: Withdraw protocol treasury
  // - 4: Change treasury address
  // - 5: Change admin address.
  /// Change adjustable pool params.
  DAOAction {
    pool_in_ix: Int,
    pool_out_ix: Int,
    action_ix: Int,
    dao_out_ix: Int,
    treasury_out_ix: Int,
  }
}
