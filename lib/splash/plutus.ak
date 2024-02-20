use aiken/hash.{Blake2b_224, Hash}
use aiken/transaction/credential.{Script, VerificationKey}
use aiken/transaction/value.{AssetName, PolicyId}

pub type VerificationKeyHash =
  Hash<Blake2b_224, VerificationKey>

pub type ScriptHash =
  Hash<Blake2b_224, Script>

pub type Asset {
  policy: PolicyId,
  name: AssetName,
}

pub fn mk_asset(policy: PolicyId, name: AssetName) -> Asset {
  Asset { policy, name }
}
