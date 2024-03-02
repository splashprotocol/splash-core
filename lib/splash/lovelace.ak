use aiken/builtin
use aiken/dict
use aiken/transaction/value

pub fn lovelace_of(val: value.Value) -> Int {
  let pol = val |> value.to_dict |> dict.to_list |> builtin.head_list
  let tok = pol.2nd |> dict.to_list |> builtin.head_list
  tok.2nd
}
