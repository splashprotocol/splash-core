use aiken/dict.{Dict}
use aiken/list
use aiken/math
use aiken/transaction/credential.{Inline, ScriptCredential, StakeCredential}
use aiken/transaction/value.{Value}
use splash/plutus.{Asset, ScriptHash}

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

/// Checks if StableSwap invariant is changed in the valid interval.
/// Note: input reserves must be reduced to equal denominator (precision):
pub fn check_invariant(
  n: Int,
  an2n: Int,
  d: Int,
  dn1: Int,
  tradable_balances_calc_before: List<Int>,
  tradable_balances_calc_after: List<Int>,
) -> Bool {
  let nn = math.pow(n, n)
  let total_prod_calc_before = prod(tradable_balances_calc_before)

  let total_prod_calc_after = prod(tradable_balances_calc_after)

  let an2n_nn = an2n - nn
  let alpha_before = an2n_nn * total_prod_calc_before
  let beta_before =
    an2n * total_prod_calc_before * sum(tradable_balances_calc_before)

  let alpha_after = an2n_nn * total_prod_calc_after
  let beta_after =
    an2n * total_prod_calc_after * sum(tradable_balances_calc_after)
  dn1 + alpha_before * d - beta_before >= 0 && dn1 + alpha_after * d - beta_after <= 0
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
