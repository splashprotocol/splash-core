use aiken/collection/dict.{Dict}
use aiken/crypto.{Blake2b_256, Hash}
use cardano/assets.{AssetName, PolicyId}

pub type Asset {
  policy: PolicyId,
  name: AssetName,
}

pub type DatumHashMap =
  Dict<Hash<Blake2b_256, Data>, Data>

pub fn mk_asset(policy: PolicyId, name: AssetName) -> Asset {
  Asset { policy, name }
}
