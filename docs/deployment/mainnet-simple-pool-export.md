# Ordinary pool mainnet export

The 13-script royalty bundle is only the royalty release. The guarded Plutarch
sources also include `PPool.hs` and `PPoolBFee.hs`. Export their validators
separately with `export-simple-pools`; do not append them to the existing
13-entry royalty manifest or substitute old deployed hashes.

The command exports exactly two raw Plutus V2 CBOR files:

| File | Source symbol |
| --- | --- |
| `pool.uplc` | `PValidators.poolValidator` (`PPool.poolValidatorT`) |
| `pool-bfee.uplc` | `PValidators.poolBFeeValidator` (`PPoolBFee.poolBFeeValidatorT`) |

On the remote Linux build machine, from a clean checkout of the reviewed
`bromel777/fix_swap_path` commit:

```bash
git rev-parse HEAD
git status --porcelain
ghc --version
cabal --version
cabal run export-simple-pools -- artifacts/mainnet-simple-pools-2026-10-07/raw-cbor
python3 scripts/verify_mainnet_simple_pool_artifacts.py artifacts/mainnet-simple-pools-2026-10-07/raw-cbor
```

The output directory must be empty. Save the command output and the two `.uplc`
files plus `script-hashes.txt` together. The verifier recalculates each Plutus
V2 hash from the exported bytes and prints a SHA-256 digest for each file.
Transfer the unmodified directory to the local machine for independent
verification before preparing any Mainnet reference-script transaction.

This export contains no DAO policy or administrator keys. The changed
`PFeeSwitch.hs` and `PFeeSwitchBFee.hs` are separate parameterized DAO-policy
sources; their production administrator PKHs and release scope need an
independent export and review. Likewise, the legacy runtime names
`constFnPoolV1`, `constFnPoolV2`, and other pool variants must be mapped to
their exact on-chain bytes before wiring a new hash. This two-script export
alone does not establish that all ordinary pool families are ready.
