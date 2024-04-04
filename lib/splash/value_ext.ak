use aiken/builtin
use aiken/dict
use aiken/list
use aiken/transaction/value.{PolicyId}

pub fn lovelace_of(val: value.Value) -> Int {
  let pol = val |> value.to_dict |> dict.to_list |> builtin.head_list
  let tok = pol.2nd |> dict.to_list |> builtin.head_list
  tok.2nd
}

pub fn lovelace_of_tail(
  val: value.Value,
) -> (Int, List<(PolicyId, dict.Dict<ByteArray, Int>)>) {
  expect [pol, ..rem] = val |> value.to_dict |> dict.to_list
  let tok = pol.2nd |> dict.to_list |> builtin.head_list
  (tok.2nd, rem)
}

pub fn quantity_of(
  val: List<(PolicyId, dict.Dict<ByteArray, Int>)>,
  pol: PolicyId,
  tn: ByteArray,
  max_policies: Int,
) -> Int {
  expect max_policies > 0
  when val is {
    [] -> 0
    [(that_pol, tokens), ..tail] ->
      if that_pol == pol {
        expect [(that_tn, q), ..other_tokens] = dict.to_list(tokens)
        if that_tn == tn {
          q
        } else {
          when other_tokens is {
            [] -> 0
            [(that_tn_1, q_1)] ->
              if that_tn_1 == tn {
                q_1
              } else {
                0
              }
            _ -> fail
          }
        }
      } else {
        quantity_of(tail, pol, tn, max_policies - 1)
      }
  }
}
