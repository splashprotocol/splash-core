# Mainnet royalty script set

Updated: 2026-10-06. This is the intended **reference-script publication set** for the protected single and double royalty pools. It is a list of script roles and output file names, not a ready-to-submit artifact bundle. Fill the final hash column of the release manifest only from CBOR rebuilt after the DAO successor-NFT fix, then verify each hash from the bytes.

## Scripts to publish

| # | Release artifact (`.uplc`) | Source/export symbol | Role |
| --- | --- | --- | --- |
| 1 | `royalty-pool` | `PValidators.royaltyPoolValidator` | Single royalty pool spending validator |
| 2 | `double-royalty-pool` | `PValidators.doubleRoyaltyPoolValidator` | Double royalty pool spending validator |
| 3 | `royalty-deposit` | `PValidators.royaltyDepositValidator` | Single royalty deposit order |
| 4 | `royalty-redeem` | `PValidators.royaltyRedeemValidator` | Single royalty redeem order |
| 5 | `double-royalty-deposit` | `PValidators.doubleRoyaltyDepositValidator` | Double royalty deposit order |
| 6 | `double-royalty-redeem` | `PValidators.doubleRoyaltyRedeemValidator` | Double royalty redeem order |
| 7 | `royalty-dao-v1-policy` | `PMintingValidators.royaltyPoolDAOV1Validator` | Single royalty DAO staking policy, parameterized by the six production Ed25519 verification keys, threshold `4`, `lpFeeIsEditable=True` |
| 8 | `double-royalty-dao-v1-policy` | `PMintingValidators.doubleRoyaltyPoolDAOV1Validator` | Double royalty DAO staking policy with the same production parameters |
| 9 | `royalty-dao-v1-action-order` | `PValidators.royaltyPooldaoV1ActionOrderValidatorFor` applied to **final** artifact #7 hash | Single royalty DAO request validator |
| 10 | `double-royalty-dao-v1-action-order` | `PValidators.royaltyPooldaoV1ActionOrderValidatorFor` applied to **final** artifact #8 hash | Double royalty DAO request validator |
| 11 | `royalty-withdraw-order` | `PValidators.royaltyWithdrawOrderValidator` | Single royalty withdrawal request validator |
| 12 | `double-royalty-withdraw-order` | `PValidators.doubleRoyaltyWithdrawOrderValidator` | Double royalty withdrawal request validator |
| 13 | `royalty-withdraw-pool-policy` | `PMintingValidators.royaltyWithdrawPoolValidator` | Single royalty withdrawal staking policy; the single pool script commits to its hash |

`app/ExportRoyaltyDaoV1.hs` now exports the complete 13-script set in mainnet mode. It pins the six ordered production verification keys, threshold `4`, and `lpFeeIsEditable=True`, and refuses a nonempty output directory. It derives each DAO action-order from the freshly built matching DAO policy hash. `test/Spec.hs` also derives both DAO action-orders dynamically, but the standalone exporter is the release entry point because it validates the input manifest and starts from a clean directory.

Remote build and export from the reviewed source revision:

```bash
cabal run export-royalty-dao-v1 -- deployments/mainnet/royalty-dao-v1-admins-2026-10-06.json artifacts/mainnet-royalty-2026-10-06/raw-cbor
python3 scripts/verify_mainnet_royalty_artifacts.py artifacts/mainnet-royalty-2026-10-06/raw-cbor
git rev-parse HEAD
ghc --version
cabal --version
```

Record the last three command outputs alongside the downloaded artifact bundle. The verifier requires `export-parameters.json` and confirms its approved production parameters, checks all 13 byte hashes, verifies the two DAO order bindings, rejects known stale/test DAO artifacts, and checks the other nine hashes against the previously reviewed guarded baseline. If an intentional change to one of those nine scripts changes its hash, review and update the baseline before accepting the export.

### Existing CBOR directories are not a release bundle

- The archived `2026-10-05/raw-cbor` bundle has production-key DAO policy hashes `56e45b69...` and `591b89ca...`. Its pool CBOR corresponds by recorded source revision to the guarded pool sources, but its DAO policies predate the successor-NFT fix.
- `/Users/aleksandr/newScripts` is **mixed**: its two pool CBOR files are byte-identical to the archive, while its DAO policies hash to the Preprod test values `90052675...` and `f687c733...`. Its single DAO order still hashes to archived `2073f4c8...`. Never deploy this directory as one set.
- `/Users/aleksandr/newnewScripts` and `/Users/aleksandr/scriptsDaoTests/preprod-royalty-dao-v1` contain Preprod test-key DAO artifacts. The newer bundle at `artifacts/mainnet-royalty-f853526/raw-cbor` has independently verified byte hashes, production keys, and both DAO order bindings. Its source-build provenance and deployed-byte behavior still need confirmation; see `deployments/mainnet/royalty-scripts-f853526.json`.

## Separate dependency for double royalty withdrawal

`PDoubleRoyaltyPool.hs` commits to withdrawal stake policy hash `a062da72f9f3280fbed90b3e95ea943aabd7eff9c82343324a9726ad`. The policy's exact source and CBOR are not exported by this repository. Obtain and hash-verify those bytes before enabling the double royalty withdrawal flow or claiming that its deployment is complete. Add its reference UTxO to the deployment report if it is published as part of the release. Do not substitute `royalty-withdraw-pool-policy`, which is the single royalty policy.

## Artifact admission rules

1. Use only a new dated directory containing all 13 exact `.uplc` names, a 13-entry `script-hashes.txt`, `export-parameters.json`, byte SHA256 sums, source commit SHA, compiler/toolchain identity, and the ordered production public keys plus derived PKHs. No private signing material belongs in this directory.
2. Recompute each Plutus V2 hash from its raw CBOR (`Blake2b-224(0x02 || raw_script_cbor)`) and compare it with the manifest. Verify both DAO order scripts were parameterized by the two final DAO policy hashes.
3. Reject the Preprod DAO hashes `90052675bc42599a1ad33ec3b547c1890fc6f68da05212dee86c7377` and `f687c733f2a878f63d8f61d8f3a57585d5a24087843d5eca559753b2`, and their Preprod DAO order hashes `5183b498297de742b6aba40c4ad5de442dea5e6237872c4cce734cfe` and `95d1b3118dd7fd24f3e986f7952ee4dc3fd373ad8a70e0e4a6115c71`.
4. Reject the archived, pre-NFT-fix DAO hashes `56e45b69e269ac0cdae96f52c28b6ae2f1daf1b2d5777b212a55c3ee` and `591b89ca42216543df1d911df3eef1145bff5a9d15d6eff8f697810f`, plus the archived single DAO order hash `2073f4c84bd09c2d8e11018c6f106fd2e00a643e118d90167d17a1d9`. Pool, deposit, redeem, and withdrawal hashes may legitimately equal archived hashes if those bytes did not change; verify them from the final build.
5. Verify that the hash of artifact #13 equals the withdrawal stake hash compiled into `PRoyaltyPool.hs` (`e0de1016c69f04483037d4cb6cc85bb6d42bc75b80774bc5bc48eb2f` at this revision). A mismatch requires updating that constant and rebuilding the pool, not publishing incompatible bytes. Apply the same exact-hash rule to the external double withdrawal policy noted above.
6. Before mainnet submission, run the remaining positive/negative bytecode tests from [mainnet-royalty-handoff.md](mainnet-royalty-handoff.md). Publish and verify reference UTxOs, then register both DAO stake credentials and update deployment manifests. Publishing a reference script does not migrate an existing pool UTxO.
