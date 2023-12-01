use aiken/math.{abs}
use whalepools/plutus.{Asset}
use whalepools/stable3pool/types.{DepositData, PoolData, RedeemData, SwapData}

pub fn convert_pool_datum(data: Data) -> PoolData {
  expect pool_datum: PoolData = data
  pool_datum
}

pub fn convert_deposit_datum(data: Data) -> DepositData {
  expect deposit_datum: DepositData = data
  deposit_datum
}

pub fn convert_redeem_datum(data: Data) -> RedeemData {
  expect redeem_datum: RedeemData = data
  redeem_datum
}

pub fn convert_swap_datum(data: Data) -> SwapData {
  expect swap_datum: SwapData = data
  swap_datum
}

pub fn check_invariant_extremum(
  x_calc: Int,
  y_calc: Int,
  z_calc: Int,
  d: Int,
  nn: Int,
  ann: Int,
) -> Bool {
  // Note: input reserves must be reduced to a common denominator:

  let total_sum_calc = x_calc + y_calc + z_calc
  let total_prod_calc = x_calc * y_calc * z_calc
  let nn_total_prod_calc = nn * total_prod_calc
  let ann_total_sum_calc = ann * total_sum_calc
  let err_eq =
    calculate_invariant_error(
      ann_total_sum_calc: ann_total_sum_calc,
      nn_total_prod_calc: nn_total_prod_calc,
      d: d,
      ann: ann,
    )

  let err_left =
    calculate_invariant_error(
      ann_total_sum_calc: ann_total_sum_calc,
      nn_total_prod_calc: nn_total_prod_calc,
      d: d - 1,
      ann: ann,
    )

  let err_right =
    calculate_invariant_error(
      ann_total_sum_calc: ann_total_sum_calc,
      nn_total_prod_calc: nn_total_prod_calc,
      d: d + 1,
      ann: ann,
    )
  err_left > err_eq && err_right > err_eq
}

pub fn calculate_invariant_error(
  // Note: input reserves must be reduced to a common denominator:
  nn_total_prod_calc: Int,
  ann_total_sum_calc: Int,
  d: Int,
  ann: Int,
) -> Int {
  let inv_right = ann * d + d * d * d * d / nn_total_prod_calc
  let inv_left = ann_total_sum_calc + d

  abs(inv_right - inv_left)
}

test invariant_error0() {
  // Check 0 error for equal amounts.
  let (x, y, z) = (1_000_000, 1_000, 1_000_000_000_000)
  let sum = 3 * 1_000_000_000_000

  let datum =
    PoolData {
      treasury_nft: Asset { policy: #"00", name: #"00" },
      base_token: Asset { policy: #"10", name: #"01" },
      quote_token0: Asset { policy: #"20", name: #"02" },
      quote_token1: Asset { policy: #"30", name: #"03" },
      lp_token: Asset { policy: #"40", name: #"04" },
      base_token_dec: 1_000,
      quote_token0_dec: 1,
      quote_token1_dec: 1_000_000_000,
      denom: 1_000,
      precision: 1_000_000_000,
      ampl_coeff_bounds: [100, 3000],
      provider_fee_num_bounds: [1, 100],
      protocol_share_num_bounds: [100, 500],
      ampl_coeff: 150,
      provider_fee_num: 10,
      protocol_share_num: 300,
      inv_no_fees: sum,
      inv: sum,
    }

  let precision = datum.precision
  let x_decimals = datum.base_token_dec
  let y_decimals = datum.quote_token0_dec
  let z_decimals = datum.quote_token1_dec

  let x_calc = x * precision / x_decimals
  let y_calc = y * precision / y_decimals
  let z_calc = z * precision / z_decimals

  let a = datum.ampl_coeff

  // Since n=3 for stable3pool and nn = n^n, we will fix nn as 27:
  let nn = 27
  let ann = a * nn
  let total_sum_calc = x_calc + y_calc + z_calc
  let total_prod_calc = x_calc * y_calc * z_calc
  let nn_total_prod_calc = nn * total_prod_calc
  let ann_total_sum_calc = ann * total_sum_calc
  calculate_invariant_error(
    ann_total_sum_calc: ann_total_sum_calc,
    nn_total_prod_calc: nn_total_prod_calc,
    d: sum,
    ann: ann,
  ) == 0
}

test invariant_check_invariant_extremum() {
  // Check that error is mimimal at the extremum point.
  let (x, y, z) = (1_000_000, 1_000, 1_000_000_000_000)
  let sum = 3 * 1_000_000_000_000

  let datum =
    PoolData {
      treasury_nft: Asset { policy: #"00", name: #"00" },
      base_token: Asset { policy: #"10", name: #"01" },
      quote_token0: Asset { policy: #"20", name: #"02" },
      quote_token1: Asset { policy: #"30", name: #"03" },
      lp_token: Asset { policy: #"40", name: #"04" },
      base_token_dec: 1_000,
      quote_token0_dec: 1,
      quote_token1_dec: 1_000_000_000,
      denom: 1_000,
      precision: 1_000_000_000,
      ampl_coeff_bounds: [100, 3000],
      provider_fee_num_bounds: [1, 100],
      protocol_share_num_bounds: [100, 500],
      ampl_coeff: 150,
      provider_fee_num: 10,
      protocol_share_num: 300,
      inv_no_fees: sum,
      inv: sum,
    }

  let precision = datum.precision
  let x_decimals = datum.base_token_dec
  let y_decimals = datum.quote_token0_dec
  let z_decimals = datum.quote_token1_dec

  let x_calc = x * precision / x_decimals
  let y_calc = y * precision / y_decimals
  let z_calc = z * precision / z_decimals

  let a = datum.ampl_coeff
  let nn = 27
  let ann = a * nn

  check_invariant_extremum(
    x_calc: x_calc,
    y_calc: y_calc,
    z_calc: z_calc,
    d: sum,
    nn: nn,
    ann: ann,
  )
}
