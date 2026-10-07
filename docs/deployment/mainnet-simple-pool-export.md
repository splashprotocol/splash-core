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

## Copied candidate with ordinary DAO policy

On 2026-10-07 the supplied local directory
`/Users/aleksandr/newDeployments/` contained the three CBOR files,
`script-hashes.txt`, and `export-parameters.json`. The local verifier passed:

| Artifact | Plutus V2 hash | SHA-256 of CBOR |
| --- | --- | --- |
| `pool` | `368679338e9489e5c6eddc5b2d58d22dc07de3ea51416ecbbd08939d` | `c7379811eb250e4c1df2c456713cb8193d24debacfd7e6f695d5606ac53355be` |
| `pool-bfee` | `59596eea67a4295201e8bbde1ea49705177397c12a1c0424a004a219` | `e906aa30fb869e4174cdea086dd7a528d63cad105eb1a420f5376f9423e5ce7b` |
| `pool-dao-policy` | `038c5045480b4e31f8cdecfc0c49a342e8a7411035eff2184201ae93` | `a99846e992cc6dc407ae32bc11694747fee01220ab614a7ccc736368141d3c98` |

The two pool hashes match the earlier local export exactly. The DAO CBOR
contains each of the six production payment PKHs once, and the supplied
public manifest has matching verification keys, threshold 4, and editable
LP fee. The manifest's `purpose` string still says royalty DAO because the
same public administrator manifest was reused; the exported DAO script is
the ordinary `PFeeSwitch` policy, not a royalty DAO policy.

This is a byte-verified candidate. The downloaded folder does not contain
the remote `git rev-parse HEAD`, clean-tree status, or compiler versions,
and no deployed-byte behavior evaluation has been recorded for this DAO.

The legacy runtime names `constFnPoolV1`, `constFnPoolV2`, and other pool
variants must be mapped to their exact on-chain bytes before wiring a new
hash. This export alone does not establish that all ordinary pool families
are ready.
