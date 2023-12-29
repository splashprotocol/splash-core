use whalepools/stable3pool/types.{DepositData, PoolData, RedeemData}

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

/// Calculates absolute error between left and right parts of the StableSwap invariant.
pub fn calculate_invariant_error(
  nn_total_prod_calc: Int,
  ann_total_sum_calc: Int,
  d: Int,
  ann: Int,
) -> Int {
  let inv_right = ann * d + d * d * d * d / nn_total_prod_calc
  let inv_left = ann_total_sum_calc + d

  if inv_right > inv_left {
    inv_right - inv_left
  } else {
    inv_left - inv_right
  }
}

/// Checks if absolute error between left and right parts of the
/// StableSwap invariant minimal at the given point (x, y, z, d) relative to d units.
/// Note: input reserves must be reduced to equal denominator (precision):
pub fn check_invariant_extremum(
  x_calc: Int,
  y_calc: Int,
  z_calc: Int,
  d: Int,
  nn: Int,
  ann: Int,
) -> Bool {
  let total_sum_calc = x_calc + y_calc + z_calc
  let total_prod_calc = x_calc * y_calc * z_calc
  let nn_total_prod_calc = nn * total_prod_calc
  let ann_total_sum_calc = ann * total_sum_calc
  // Error at the potential "equilibrium" point:
  let err_eq =
    calculate_invariant_error(
      ann_total_sum_calc: ann_total_sum_calc,
      nn_total_prod_calc: nn_total_prod_calc,
      d: d,
      ann: ann,
    )

  // Move 1 step to the left from the reference `d` value:
  let err_left =
    calculate_invariant_error(
      ann_total_sum_calc: ann_total_sum_calc,
      nn_total_prod_calc: nn_total_prod_calc,
      d: d - 1,
      ann: ann,
    )

  // Move 1 step to the right from the reference 'd' value:
  let err_right =
    calculate_invariant_error(
      ann_total_sum_calc: ann_total_sum_calc,
      nn_total_prod_calc: nn_total_prod_calc,
      d: d + 1,
      ann: ann,
    )

  // Сompare the calculated values ​​with the error at the potential "equilibrium" point.
  // If the inequalities are satisfied, then the point is the minimum of the StableSwap invariant.
  // Note: we can afford to check the minimum only using a grid of 'd' values ​​since the
  // stableswap invariant has a single extremum in the positive quadrant and the values
  // ​​of all variable values have the same precision.
  err_left > err_eq && err_right > err_eq
}

/// Checks if absolute error between left and right parts of the
/// StableSwap invariant minimal at the given point (x, y, z, d) relative to x/y/z units.
/// Note: input reserves must be reduced to equal denominator (precision):
pub fn check_invariant_extremum_for_asset(
  x_calc: Int,
  y_calc: Int,
  z_calc: Int,
  d: Int,
  nn: Int,
  ann: Int,
  minimal_asset_unit: Int,
  asset_ind: Int,
) -> Bool {
  let total_sum_calc = x_calc + y_calc + z_calc
  let total_prod_calc = x_calc * y_calc * z_calc
  let nn_total_prod_calc = nn * total_prod_calc
  let ann_total_sum_calc = ann * total_sum_calc
  // Error at the potential "equilibrium" point:
  let err_eq =
    calculate_invariant_error(
      ann_total_sum_calc: ann_total_sum_calc,
      nn_total_prod_calc: nn_total_prod_calc,
      d: d,
      ann: ann,
    )

  // Move 1 step to the left from the reference asset balance:
  let (nn_total_prod_calc_left, nn_total_prod_calc_right) =
    if asset_ind == 0 {
      let total_prod_no_x = total_prod_calc / x_calc
      (
        nn * total_prod_no_x * ( x_calc - 2 * minimal_asset_unit ),
        nn * total_prod_no_x * ( x_calc + 2 * minimal_asset_unit ),
      )
    } else if asset_ind == 1 {
      let total_prod_no_y = total_prod_calc / y_calc
      (
        nn * total_prod_no_y * ( y_calc - 2 * minimal_asset_unit ),
        nn * total_prod_no_y * ( y_calc + 2 * minimal_asset_unit ),
      )
    } else {
      let total_prod_no_z = total_prod_calc / z_calc
      (
        nn * total_prod_no_z * ( z_calc - 2 * minimal_asset_unit ),
        nn * total_prod_no_z * ( z_calc + 2 * minimal_asset_unit ),
      )
    }

  let err_left =
    calculate_invariant_error(
      ann_total_sum_calc: ann_total_sum_calc - ann * 2 * minimal_asset_unit,
      nn_total_prod_calc: nn_total_prod_calc_left,
      d: d,
      ann: ann,
    )

  // Move 1 step to the right from the reference 'd' value:
  let err_right =
    calculate_invariant_error(
      ann_total_sum_calc: ann_total_sum_calc + ann * 2 * minimal_asset_unit,
      nn_total_prod_calc: nn_total_prod_calc_right,
      d: d,
      ann: ann,
    )

  // Сompare the calculated values ​​with the error at the potential "equilibrium" point.
  // If the inequalities are satisfied, then the point is the minimum of the StableSwap invariant.
  // Note: we can afford to check the minimum only using a grid of 'd' values ​​since the
  // stableswap invariant has a single extremum in the positive quadrant and the values
  // ​​of all variable values have the same precision.
  err_left > err_eq && err_right > err_eq
}
