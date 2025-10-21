use aiken/collection/list
use cardano/assets.{Value}
use splash/plutus.{Asset}

pub fn asset_quantity_of(value: Value, asset: Asset) -> Int {
  let Asset { policy, name: token_name } = asset
  assets.quantity_of(value, policy, token_name)
}

pub fn v_len(v: Value) -> Int {
  v |> assets.flatten |> list.length
}
