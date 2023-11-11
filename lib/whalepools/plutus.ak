use aiken/hash.{Blake2b_224, Hash}
use aiken/transaction/credential.{VerificationKey}
use aiken/transaction/value.{AssetName, PolicyId}

pub type VerificationKeyHash =
  Hash<Blake2b_224, VerificationKey>

pub type Asset {
  policy: PolicyId,
  name: AssetName,
}
