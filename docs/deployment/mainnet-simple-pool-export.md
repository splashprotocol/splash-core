# Ordinary pool mainnet export

The 13-script royalty bundle is only the royalty release. The guarded Plutarch
sources also include `PPool.hs` and `PPoolBFee.hs`. Export their validators
separately with `export-simple-pools`; do not append them to the existing
13-entry royalty manifest or substitute old deployed hashes.

The command exports three raw Plutus V2 CBOR files:

| File | Source symbol |
| --- | --- |
| `pool.uplc` | `PValidators.poolValidator` (`PPool.poolValidatorT`) |
| `pool-bfee.uplc` | `PValidators.poolBFeeValidator` (`PPoolBFee.poolBFeeValidatorT`) |
| `pool-dao-policy.uplc` | `PMintingValidators.daoMintPolicyValidator` (`PFeeSwitch.daoMultisigPolicyValidatorT`) |

The ordinary DAO policy is a staking policy for `pool.uplc`. It does not
embed the pool script hash: `PPool` reads the DAO staking credential from its
datum. `pool-bfee.uplc` has a different datum layout (`feeNumX`, `feeNumY`).
The current `PFeeSwitchBFee.hs` is not wired into `PMintingValidators` and
is not part of this export. Do not configure a new bidirectional-fee pool
with `pool-dao-policy` until a matching BFee DAO implementation has been
built and tested.

The ordinary DAO policy takes the six 28-byte payment PKHs in the public
administrator manifest, with threshold 4 and editable LP fee. This is
different from the six raw 32-byte verification-key parameters embedded in
the Royalty DAO V1 policies. The exporter pins the approved PKH values and
rejects a mismatched Mainnet manifest.

On the remote Linux build machine, from a clean checkout of the reviewed
`bromel777/fix_swap_path` commit:

```bash
git rev-parse HEAD
git status --porcelain
ghc --version
cabal --version
cd plutarch-validators
cabal run export-simple-pools -- ../deployments/mainnet/royalty-dao-v1-admins-2026-10-06.json ../artifacts/mainnet-simple-pools-2026-10-07-with-dao/raw-cbor
python3 ../scripts/verify_mainnet_simple_pool_artifacts.py ../artifacts/mainnet-simple-pools-2026-10-07-with-dao/raw-cbor
```

The output directory must be empty. Save the command output and all three
`.uplc` files, `script-hashes.txt`, and `export-parameters.json` together. The
verifier recalculates each Plutus V2 hash from the exported bytes and prints
a SHA-256 digest for each file. It also checks the pinned Mainnet DAO PKHs.
Transfer the unmodified directory to the local machine for independent
verification before preparing any Mainnet reference-script transaction.

The earlier two-script bundle in `mainnet-simple-pools-2026-10-07/raw-cbor`
does not include the ordinary DAO policy. Its verified pool hashes are
`368679338e9489e5c6eddc5b2d58d22dc07de3ea51416ecbbd08939d` and
`59596eea67a4295201e8bbde1ea49705177397c12a1c0424a004a219`.
These are byte-verified from the local copy, but the remote source revision
and build environment still need attestation. The new exporter recompiles
the same pool validators; compare the two resulting hashes with this baseline.

The legacy runtime names `constFnPoolV1`, `constFnPoolV2`, and other pool
variants must be mapped to their exact on-chain bytes before wiring a new
hash. This export alone does not establish that all ordinary pool families
are ready.
