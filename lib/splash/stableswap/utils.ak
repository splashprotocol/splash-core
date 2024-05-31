use aiken/dict.{Dict}
use aiken/list
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
  nn: Int,
  an2n: Int,
  alpha_before: Int,
  betta_before: Int,
  d: Int,
  dn1: Int,
  total_sum_calc_after: Int,
  total_prod_calc_after: Int,
) -> Bool {
  let alpha_after = ( an2n - nn ) * total_prod_calc_after
  let betta_after = an2n * total_prod_calc_after * total_sum_calc_after

  if alpha_after >= alpha_before {
    dn1 + alpha_before * d - betta_before >= 0 && dn1 + alpha_after * d - betta_after <= 0
  } else {
    dn1 + alpha_before * d - betta_before <= 0 && dn1 + alpha_after * d - betta_after >= 0
  }
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
