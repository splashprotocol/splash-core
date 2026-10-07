# Preprod DAO V1 mock configuration

This directory contains only public raw Ed25519 verification keys, their Cardano payment-key hashes, and metadata for
six disposable DAO administrators used on Preprod. The corresponding BIP-39
mnemonics are held locally in `.private/` with mode `0600`; they are never
committed, copied into manifests, or used on Mainnet.

Generate raw parameterized policy bytes after generating the public manifest:

```bash
cabal run export-royalty-dao-v1 -- \
  deployments/preprod/royalty-dao-v1-mock-admins-2026-10-05.json \
  artifacts/preprod-royalty-dao-2026-10-05/raw-cbor
```

The output contains the actual `royaltyPoolDAOV1Validator` and
`doubleRoyaltyPoolDAOV1Validator` policy CBOR, parameterized with the six
public administrator verification keys, a 4-of-6 threshold, and editable LP fee. It is
not the generic FeeSwitch DAO policy.

## Parameter semantics

`daoAdminVerificationKeys` is an ordered list of 32-byte Ed25519 public keys, hex encoded. These bytes are the DAO V1 policy parameters because the validator calls `verifyEd25519Signature` on them directly. `paymentKeyHashes` are 28-byte Blake2b-224 hashes provided only as Cardano identity metadata; they must never be substituted for the verification-key list.
