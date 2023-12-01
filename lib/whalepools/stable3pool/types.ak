use whalepools/plutus.{Asset, VerificationKeyHash}

pub type PoolData {
  PoolData {
    // Identifier of the treasury:
    treasury_nft: Asset,
    // Identifier of the base asset:
    base_token: Asset,
    // Identifier of the first quote asset:
    quote_token0: Asset,
    // Identifier of the second quote asset:
    quote_token1: Asset,
    // Identifier of the liquidity token asset:
    lp_token: Asset,
    // Decimals of the base asset:
    base_token_dec: Int,
    // Decimals of the first quote asset:
    quote_token0_dec: Int,
    // Decimals of the second quote asset:
    quote_token1_dec: Int,
    // Common denominator of calculations:
    denom: Int,
    // Precision multiplier for calculations:
    precision: Int,
    // Bounds of the ampl_coeff: [lower_bound, upper_bound]:
    ampl_coeff_bounds: List<Int>,
    // Bounds of the provider_fee_num : [lower_bound, upper_bound]:
    provider_fee_num_bounds: List<Int>,
    // Bounds of the protocol_share_num : [lower_bound, upper_bound]:
    protocol_share_num_bounds: List<Int>,
    // Invariant's amplification coefficient:
    ampl_coeff: Int,
    // Numerator of the swap fee:
    provider_fee_num: Int,
    // Numerator of the protocol fee share:
    protocol_share_num: Int,
    // Value of the pool's invariant before applying fees to the last operation with pool's liquidity:
    inv_no_fees: Int,
    // Actual value of the pool's invariant:
    inv: Int,
  }
}

pub type DepositData {
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

pub type SwapData {
  SwapData {
    // Identifier of the pool:
    pool_nft: Asset,
    // Redeemer PK:
    redeemer: VerificationKeyHash,
    // Expected quote token identifier:
    expected_quote_token: Asset,
    // Minimum expected amount of quote tokens:
    min_expected_quote_amount: Int,
  }
}
