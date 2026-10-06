# Royalty pool mainnet deployment handoff

Updated: 2026-10-06. This document records the current state for agents continuing the deployment work. It is a status record, not evidence that the contracts are ready for mainnet funds.

The exact publication allowlist and artifact rejection rules are in [mainnet-royalty-script-list.md](mainnet-royalty-script-list.md).

## Source and artifact status

- The protected pool validators are `plutarch-validators/WhalePoolsDex/PContracts/PRoyaltyPool.hs` and `PDoubleRoyaltyPool.hs`. Their previously exported pool hashes are `72ed0eaa3594bfadd006992ed574dee50bbced070955c0c296fb8f8e` and `ca6c752e43e47c6287d137b6d3f0f0d22a5c23b61910e3abcdcf81b2`. Recompute these from each final export; do not assume they remain unchanged.
- The DAO policies are `PRoyaltyDAOV1.hs` and `PDoubleRoyaltyDAOV1.hs`. The current release source adds `(correctPoolNftInNewPool #== 1)` to each final policy predicate. These changes have not yet been rebuilt in the remote Cabal environment. They change the DAO policy bytes and hashes. DAO action-order scripts parameterized by the DAO policy hashes must also be regenerated.
- `artifacts/archive/mainnet-royalty-deployment-2026-10-05/` preserves a pre-fix CBOR bundle and its SHA256 sums. Its DAO policy hashes `56e45b69...` and `591b89ca...` predate the successor NFT check. Keep this directory immutable; it is not a release candidate.
- `deployments/preprod/royalty-dao-v1-mock-admins-2026-10-05.json` contains disposable Preprod DAO public keys. The deployed Preprod policies `90052675...` and `f687c733...` are test-key artifacts and must not be deployed on mainnet.
- `plutarch-validators/app/ExportRoyaltyDaoV1.hs` accepts either Preprod or Mainnet manifests. Mainnet mode requires the pinned six production verification keys in order, threshold `4`, and `lpFeeIsEditable=True`; it exports all 13 royalty scripts to an empty directory. It takes no private keys or mnemonics. The production input is `deployments/mainnet/royalty-dao-v1-admins-2026-10-06.json` and the independent byte verifier is `scripts/verify_mainnet_royalty_artifacts.py`.
- Mainnet DAO identities must be the authorized production administrators. For these Royalty DAO V1 policies, `pverifyEd25519Signature` receives the full 32-byte verification keys; 28-byte payment PKHs are identity metadata and cannot replace the policy parameters. Record and cross-check each production PKH against its corresponding verification key, retaining the exact key order. Older FeeSwitch DAO policies use PKHs in `txInfo.signatories`; do not apply that parameter format to Royalty DAO V1.

### Candidate production DAO keys verified on 2026-10-06

The six keys supplied by the operator match `plutarch-validators/test/Spec.hs` in the same order. That test exports both DAO policies with threshold `4` and `lpFeeIsEditable=True`. Each key is valid 32-byte hex, all six are distinct, and each raw key occurs exactly once in each DAO policy CBOR in the archived mainnet bundle. SHA256 verification of the archive passed. Independently computed Plutus V2 hashes of its two DAO policies match `56e45b69e269ac0cdae96f52c28b6ae2f1daf1b2d5777b212a55c3ee` and `591b89ca42216543df1d911df3eef1145bff5a9d15d6eff8f697810f`. This verifies archived byte provenance, not control of the signing keys or the post-NFT-fix policy hashes.

| Index | Raw Ed25519 verification key | Derived Blake2b-224 payment PKH |
| --- | --- | --- |
| 1 | `0bb1d2db22f9b641f0afe8d8a398279cb778d8f86167500f7e63ebbdc35b4d69` | `68aa59a87dbdbf8f78386dc6b83e63149d8c13939a1b0cda39707f9a` |
| 2 | `4e8221615500dbf6737b02992610ffeed82da6826dc3d9729febf1d32d766615` | `518a9c32deedc0b82604972692a2b7eb6c10b020d77c3c72e764b156` |
| 3 | `ae536160ccec4f078982396125773d509072397e35ed6fab7af2a762ca147318` | `f3a12554ca0ccd1a220b1de839a1940bc1006f08768b636fa98c66c9` |
| 4 | `04e3a257bcb0306c27e796bc16d1b7bde8f2306dc1d6aa344f6043ef48bd7fd8` | `0ff61bd4fdda8767414642f83f1be47bd674381ef32fc0e8164aa37d` |
| 5 | `83d3aa4ccd1c72ff7a27c032394f44b699778b6050290e37fd82bc295f5caf18` | `d350803d45e327f8808469d5dde9e0f6fdc6e6637d85ed44cc37a12c` |
| 6 | `a7d30e99673c57638bdb65b5a0554ddee3135131940a41bbd3534b0d4c709506` | `3703634e3e34c0e7fd9a7348ad213f9272279535c73f4ec00efe00bd` |

## Existing deployment tooling

- The most relevant publisher is `/Users/aleksandr/IdeaProjects/spectrum-offchain-multiplatform/splash-testing-cardano/scripts/deploy-preprod-royalty-reference-scripts.ts`. It reads raw `.uplc` files and `script-hashes.txt`, recomputes Plutus V2 script hashes, batches reference scripts with a 10,000-byte payload budget, saves signed pending CBOR before broadcast, verifies reference-script hashes through Blockfrost, and writes a `txHash#index` deployment report. It is hardcoded to Preprod network, API, project-ID environment variable, and wallet seed environment variable. Adapt and review it for mainnet; do not run it unchanged.
- `/Users/aleksandr/IdeaProjects/spectrum-offchain-multiplatform/splash-testing-cardano/src/deploy.ts` is an older general publisher driven by `plutus.ts`. Its active `deploy()` body publishes only one reference output and registers factory stake. Its `getDeployedValidators()` assumes every built validator corresponds to a sequential output. It is not the royalty release publisher.
- The Preprod scripts for DAO stake registration, test-pool creation, and DAO request submission under `splash-testing-cardano/scripts/` have hardcoded test hashes and are not mainnet deployment scripts.
- Mainnet deployment consumers include `bloom-cardano-agent/resources/mainnet.deployment.json` in spectrum-offchain-multiplatform. Update manifests and runtime wiring only after confirmed reference outputs and exact hash verification. Preserve support for older pool versions.

## Validation status and release work

- Source review found the negative-reserve and swap-direction guards in ordinary Swap/Deposit/Redeem branches. It also found the missing DAO successor NFT check noted above; a second review found that the added predicate closes that specific gap. This is source-level review only.
- Preprod evidence exists for protected royalty pool creation and DAO `ChangeTreasuryFee` on both versions. It does not establish a full Deposit/Swap/Redeem/WithdrawTreasury matrix or deployed-byte rejection of the known invalid transitions.
- Before a mainnet release: commit and build the NFT fix in the pinned remote Cabal environment; run the standalone mainnet exporter and independent verifier as described in [mainnet-royalty-script-list.md](mainnet-royalty-script-list.md); retain byte SHA256 sums and the exact source revision; run positive and negative tests against those exact bytes; then publish reference scripts and register DAO stake credentials. Record confirmed `txHash#index` for each script.
- The double royalty pool references a withdrawal stake hash `a062da72f9f3280fbed90b3e95ea943aabd7eff9c82343324a9726ad`. Confirm the exact policy bytes/source and hash before treating double royalty withdrawal as validated.
- Pool creation needs an off-chain check of datum asset identities, reserve backing, fee configuration, DAO credentials, and initial NFT/LP balances. The NFT mint policy alone does not establish all these properties.

Do not place production private keys, seed phrases, or signed transactions in the repository. Production DAO public verification keys can be recorded in a reviewed public manifest.
