use aiken/dict.{Dict}
use aiken/list
use aiken/math
use aiken/transaction.{Datum, DatumHash, InlineDatum, NoDatum}
use aiken/transaction/credential.{Inline, ScriptCredential, StakeCredential}
use aiken/transaction/value.{Value}
use splash/plutus.{Asset, DatumHashMap, ScriptHash}

/// Calculates element-wise subtraction of two lists.
pub fn lists_diff(lst0: List<Int>, lst1: List<Int>) -> List<Int> {
  list.map2(lst0, lst1, fn(a, b) { a - b })
}

test test_lists_diff() {
  let (lst0, lst1) = ([0, 1, 2], [1, 2, 0])

  lists_diff(lst0, lst1) == [-1, -1, 2]
}

/// Calculates element-wise absolute subtraction of two lists.
pub fn lists_abs_diff(lst0: List<Int>, lst1: List<Int>) -> List<Int> {
  list.map2(
    lst0,
    lst1,
    fn(a, b) {
      if a > b {
        a - b
      } else {
        b - a
      }
    },
  )
}

test test_lists_abs_diff() {
  let (lst0, lst1) = ([0, 1, 2], [1, 2, 0])

  lists_abs_diff(lst0, lst1) == [1, 1, 2]
}

/// Calculates element-wise multiplication of two lists.
pub fn lists_mult(lst0: List<Int>, lst1: List<Int>) -> List<Int> {
  list.map2(lst0, lst1, fn(a, b) { a * b })
}

test test_lists_mult() {
  let (lst0, lst1) = ([2, 4, 6], [1, 2, 3])

  lists_mult(lst0, lst1) == [2, 8, 18]
}

/// Calculates element-wise division of two lists.
pub fn lists_div(lst0: List<Int>, lst1: List<Int>) -> List<Int> {
  list.map2(lst0, lst1, fn(a, b) { a / b })
}

test test_lists_div() {
  let (lst0, lst1) = ([2, 4, 6], [1, 2, 3])

  lists_div(lst0, lst1) == [2, 2, 2]
}

/// Checks if all list values in list are greater than zero.
pub fn list_is_positive(lst: List<Int>) -> Bool {
  let (lst_positive, _) = list.partition(lst, fn(x) { x > 0 })
  list.length(lst) == list.length(lst_positive)
}

test test_list_is_positive() {
  let (lst0, lst1, lst2) = ([0, 1, -2, 3], [1, 1, 2, 3], [1, 1, 0, 3])

  list_is_positive(lst0) == False && list_is_positive(lst1) == True && list_is_positive(
    lst2,
  ) == False
}

/// Calculates sum of all list elements.
pub fn sum(lst: List<Int>) -> Int {
  list.reduce(lst, 0, fn(a, b) { a + b })
}

test test_list_sum() {
  let (lst0, lst1, lst2) = ([0, 0], [1, 2, 0], [0, 0, 3, 4, 0])

  sum(lst0) == 0 && sum(lst1) == 3 && sum(lst2) == 7
}

/// Calculates non-zero product of all list elements.
/// Assert Sum() > 0!!!
pub fn prod(lst: List<Int>) -> Int {
  let lst_non_zero =
    list.filter_map(
      lst,
      fn(x) {
        if x != 0 {
          Some(x)
        } else {
          None
        }
      },
    )
  list.reduce(lst_non_zero, 1, fn(a, b) { a * b })
}

test test_list_prod() {
  let (lst0, lst1, lst2) = ([0, 0], [1, 2, 0], [0, 0, 3, 4, 0])

  prod(lst0) == 1 && prod(lst1) == 2 && prod(lst2) == 12
}

/// Calculates absolute error between left and right parts of the StableSwap invariant.
pub fn calculate_invariant_error(
  n: Int,
  ann: Int,
  d: Int,
  nn_total_prod_calc: Int,
  ann_total_sum_calc: Int,
) -> Int {
  let inv_right = ann * d + math.pow(d, n + 1) / nn_total_prod_calc
  let inv_left = ann_total_sum_calc + d

  if inv_right > inv_left {
    inv_right - inv_left
  } else {
    inv_left - inv_right
  }
}

test test_calculate_invariant_error() {
  let a = 1123
  let balance = 899_612_337_112_490
  let (n0, n1, n2) = (2, 3, 4)

  let (nn0, nn1, nn2) = (math.pow(n0, n0), math.pow(n1, n1), math.pow(n2, n2))
  let (ann0, ann1, ann2) = (a * nn0, a * nn1, a * nn2)

  let (balances_calc0, balances_calc1, balances_calc2) =
    (
      list.repeat(balance, n0),
      list.repeat(balance, n1),
      list.repeat(balance, n2),
    )
  let (p0, p1, p2) =
    (prod(balances_calc0), prod(balances_calc1), prod(balances_calc2))
  let (s0, s1, s2) =
    (sum(balances_calc0), sum(balances_calc1), sum(balances_calc2))
  let (nn_total_prod_calc0, nn_total_prod_calc1, nn_total_prod_calc2) =
    (nn0 * p0, nn1 * p1, nn2 * p2)
  let (ann_total_sum_calc0, ann_total_sum_calc1, ann_total_sum_calc2) =
    (ann0 * s0, ann1 * s1, ann2 * s2)

  calculate_invariant_error(
    n0,
    ann0,
    s0,
    nn_total_prod_calc0,
    ann_total_sum_calc0,
  ) == 0 && calculate_invariant_error(
    n1,
    ann1,
    s1,
    nn_total_prod_calc1,
    ann_total_sum_calc1,
  ) == 0 && calculate_invariant_error(
    n2,
    ann2,
    s2,
    nn_total_prod_calc2,
    ann_total_sum_calc2,
  ) == 0
}

/// Calculates absolute error between left and right parts of the StableSwap invariant (d^(n+1) is passed explicitly).
pub fn calculate_invariant_error_for_swap(
  ann: Int,
  d: Int,
  dn1: Int,
  nn_total_prod_calc: Int,
  ann_total_sum_calc: Int,
) -> Int {
  let inv_right = ann * d + dn1 / nn_total_prod_calc
  let inv_left = ann_total_sum_calc + d

  if inv_right > inv_left {
    inv_right - inv_left
  } else {
    inv_left - inv_right
  }
}

test test_calculate_invariant_error_for_swap() {
  let a = 1123
  let balance = 899_612_337_112_490
  let (n0, n1, n2) = (2, 3, 4)

  let (nn0, nn1, nn2) = (math.pow(n0, n0), math.pow(n1, n1), math.pow(n2, n2))
  let (ann0, ann1, ann2) = (a * nn0, a * nn1, a * nn2)

  let (balances_calc0, balances_calc1, balances_calc2) =
    (
      list.repeat(balance, n0),
      list.repeat(balance, n1),
      list.repeat(balance, n2),
    )
  let (p0, p1, p2) =
    (prod(balances_calc0), prod(balances_calc1), prod(balances_calc2))
  let (s0, s1, s2) =
    (sum(balances_calc0), sum(balances_calc1), sum(balances_calc2))
  let (dn10, dn11, dn12) =
    (math.pow(s0, n0 + 1), math.pow(s1, n1 + 1), math.pow(s2, n2 + 1))
  let (nn_total_prod_calc0, nn_total_prod_calc1, nn_total_prod_calc2) =
    (nn0 * p0, nn1 * p1, nn2 * p2)
  let (ann_total_sum_calc0, ann_total_sum_calc1, ann_total_sum_calc2) =
    (ann0 * s0, ann1 * s1, ann2 * s2)

  calculate_invariant_error_for_swap(
    ann0,
    s0,
    dn10,
    nn_total_prod_calc0,
    ann_total_sum_calc0,
  ) == 0 && calculate_invariant_error_for_swap(
    ann1,
    s1,
    dn11,
    nn_total_prod_calc1,
    ann_total_sum_calc1,
  ) == 0 && calculate_invariant_error_for_swap(
    ann2,
    s2,
    dn12,
    nn_total_prod_calc2,
    ann_total_sum_calc2,
  ) == 0
}

/// Checks if absolute error between left and right parts of the
/// StableSwap invariant minimal at the given point (x, y, z, d) relative to d units.
/// Note: input reserves must be reduced to equal denominator (precision):
pub fn check_invariant_extremum(
  balances_calc: List<Int>,
  n: Int,
  nn: Int,
  ann: Int,
  d: Int,
) -> Bool {
  let total_sum_calc = sum(balances_calc)
  let total_prod_calc = prod(balances_calc)
  let nn_total_prod_calc = nn * total_prod_calc
  let ann_total_sum_calc = ann * total_sum_calc

  // Error at the potential "equilibrium" point:
  let err_eq =
    calculate_invariant_error(
      n: n,
      ann: ann,
      d: d,
      ann_total_sum_calc: ann_total_sum_calc,
      nn_total_prod_calc: nn_total_prod_calc,
    )

  // Move 1 step to the left from the reference `d` value:
  let err_left =
    calculate_invariant_error(
      n: n,
      ann: ann,
      d: d - 1,
      ann_total_sum_calc: ann_total_sum_calc,
      nn_total_prod_calc: nn_total_prod_calc,
    )

  // Move 1 step to the right from the reference 'd' value:
  let err_right =
    calculate_invariant_error(
      n: n,
      ann: ann,
      d: d + 1,
      ann_total_sum_calc: ann_total_sum_calc,
      nn_total_prod_calc: nn_total_prod_calc,
    )

  // Сompare the calculated values ​​with the error at the potential "equilibrium" point.
  // If the inequalities are satisfied, then the point is the minimum of the StableSwap invariant.
  // Note: we can afford to check the minimum only using a grid of 'd' values ​​since the
  // stableswap invariant has a single extremum in the positive quadrant and the values
  // ​​of all variable values have the same precision.
  err_left > err_eq && err_right > err_eq
}

test test_check_invariant_extremum() {
  let a = 3942
  let balance = 554_932_728_654_229
  let (n0, n1, n2) = (2, 3, 4)

  let (nn0, nn1, nn2) = (math.pow(n0, n0), math.pow(n1, n1), math.pow(n2, n2))
  let (ann0, ann1, ann2) = (a * nn0, a * nn1, a * nn2)

  let (balances_calc0, balances_calc1, balances_calc2) =
    (
      list.repeat(balance, n0),
      list.repeat(balance, n1),
      list.repeat(balance, n2),
    )

  let (s0, s1, s2) =
    (sum(balances_calc0), sum(balances_calc1), sum(balances_calc2))

  check_invariant_extremum(balances_calc0, n0, nn0, ann0, s0) && check_invariant_extremum(
    balances_calc1,
    n1,
    nn1,
    ann1,
    s1,
  ) && check_invariant_extremum(balances_calc2, n2, nn2, ann2, s2) && !check_invariant_extremum(
    balances_calc0,
    n0,
    nn0,
    ann0,
    s0 + 1,
  ) && !check_invariant_extremum(balances_calc0, n0, nn0, ann0, s0 - 1) && !check_invariant_extremum(
    balances_calc1,
    n1,
    nn1,
    ann1,
    s1 + 1,
  ) && !check_invariant_extremum(balances_calc1, n1, nn1, ann1, s1 - 1) && !check_invariant_extremum(
    balances_calc2,
    n2,
    nn2,
    ann2,
    s2 + 1,
  ) && !check_invariant_extremum(balances_calc2, n2, nn2, ann2, s2 - 1)
}

/// Checks if absolute error between left and right parts of the
/// StableSwap invariant minimal at the given point (x, y, z, d) relative to x/y/z units.
/// Note: input reserves must be reduced to equal denominator (precision):
pub fn check_invariant_extremum_for_asset(
  balances_calc: List<Int>,
  d: Int,
  n: Int,
  nn: Int,
  ann: Int,
  minimal_asset_unit: Int,
  asset_ind: Int,
) -> Bool {
  let total_sum_calc = sum(balances_calc)
  let total_prod_calc = prod(balances_calc)
  let nn_total_prod_calc = nn * total_prod_calc
  let ann_total_sum_calc = ann * total_sum_calc
  let dn1 = math.pow(d, n + 1)
  // Error at the potential "equilibrium" point:
  let err_eq =
    calculate_invariant_error_for_swap(
      ann: ann,
      d: d,
      dn1: dn1,
      ann_total_sum_calc: ann_total_sum_calc,
      nn_total_prod_calc: nn_total_prod_calc,
    )

  // Calculate "left" and "right" errors:
  let max_asset_calc_error = n * minimal_asset_unit
  let max_sum_calc_error = max_asset_calc_error * ann

  expect Some(target_asset_balance_calc) = list.at(balances_calc, asset_ind)
  let total_prod_no_target_asset = total_prod_calc / target_asset_balance_calc

  let (nn_total_prod_calc_left, nn_total_prod_calc_right) =
    (
      nn * total_prod_no_target_asset * (
        target_asset_balance_calc - max_asset_calc_error
      ),
      nn * total_prod_no_target_asset * (
        target_asset_balance_calc + max_asset_calc_error
      ),
    )

  let err_left =
    calculate_invariant_error_for_swap(
      ann: ann,
      d: d,
      dn1: dn1,
      ann_total_sum_calc: ann_total_sum_calc - max_sum_calc_error,
      nn_total_prod_calc: nn_total_prod_calc_left,
    )

  let err_right =
    calculate_invariant_error_for_swap(
      ann: ann,
      d: d,
      dn1: dn1,
      ann_total_sum_calc: ann_total_sum_calc + max_sum_calc_error,
      nn_total_prod_calc: nn_total_prod_calc_right,
    )

  // Сompare the calculated values ​​with the error at the potential "equilibrium" point.
  // If the inequalities are satisfied, then the point is the minimum of the StableSwap invariant.
  // Note: we can afford to check the minimum only using a grid of 'd' values ​​since the
  // stableswap invariant has a single extremum in the positive quadrant and the values
  // ​​of all variable values have the same precision.
  err_left > err_eq && err_right > err_eq
}

test test_check_invariant_extremum_for_asset() {
  let a = 5041
  let balance = 852_604_275_323_231
  let (n0, n1, n2) = (2, 3, 4)

  let (nn0, nn1, nn2) = (math.pow(n0, n0), math.pow(n1, n1), math.pow(n2, n2))
  let (ann0, ann1, ann2) = (a * nn0, a * nn1, a * nn2)

  let (balances_calc0, balances_calc0_mal, balances_calc1, balances_calc2) =
    (
      list.repeat(balance, n0),
      list.repeat(balance - 1, n0),
      list.repeat(balance, n1),
      list.repeat(balance, n2),
    )
  let (s0, s1, s2) =
    (sum(balances_calc0), sum(balances_calc1), sum(balances_calc2))

  check_invariant_extremum_for_asset(balances_calc0, s0, n0, nn0, ann0, 1, 0) && check_invariant_extremum_for_asset(
    balances_calc0,
    s0,
    n0,
    nn0,
    ann0,
    1,
    1,
  ) && check_invariant_extremum_for_asset(
    balances_calc1,
    s1,
    n1,
    nn1,
    ann1,
    1,
    0,
  ) && check_invariant_extremum_for_asset(
    balances_calc1,
    s1,
    n1,
    nn1,
    ann1,
    1,
    1,
  ) && check_invariant_extremum_for_asset(
    balances_calc1,
    s1,
    n1,
    nn1,
    ann1,
    1,
    2,
  ) && check_invariant_extremum_for_asset(
    balances_calc2,
    s2,
    n2,
    nn2,
    ann2,
    1,
    0,
  ) && check_invariant_extremum_for_asset(
    balances_calc2,
    s2,
    n2,
    nn2,
    ann2,
    1,
    1,
  ) && check_invariant_extremum_for_asset(
    balances_calc2,
    s2,
    n2,
    nn2,
    ann2,
    1,
    2,
  ) && check_invariant_extremum_for_asset(
    balances_calc2,
    s2,
    n2,
    nn2,
    ann2,
    1,
    3,
  ) && !check_invariant_extremum_for_asset(
    balances_calc0_mal,
    s0,
    n0,
    nn0,
    ann0,
    1,
    0,
  ) && !check_invariant_extremum_for_asset(
    balances_calc0_mal,
    s0,
    n0,
    nn0,
    ann0,
    1,
    1,
  )
}

/// Finds script datum in output.
pub fn find_script_datum(datums: DatumHashMap, datum: Datum) -> Data {
  let datum_opt =
    when datum is {
      NoDatum -> None
      DatumHash(dh) ->
        datums
          |> dict.get(dh)
      InlineDatum(data) -> Some(data)
    }
  expect Some(datum) = datum_opt
  datum
}

/// Extract tokens balances from the given value.
pub fn extract_target_balances(
  target_tokens_list: List<Asset>,
  v: Value,
) -> List<Int> {
  list.map(
    target_tokens_list,
    fn(token) {
      let Asset { policy, name } = token
      value.quantity_of(v, policy, name)
    },
  )
}

// Given witness script is present in withdrawals. 
pub fn contains_withdrawal(
  withdrawals: Dict<StakeCredential, Int>,
  script_hash: ScriptHash,
) -> Bool {
  let witness_scred = Inline(ScriptCredential(script_hash))
  dict.has_key(withdrawals, witness_scred)
}
