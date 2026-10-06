# Mainnet royalty deployment status — 2026-10-06

This is the starting point for the next agent. Read the [deployment handoff](mainnet-royalty-handoff.md) and [13-script allowlist](mainnet-royalty-script-list.md) for the broader context.

## Current artifacts

- Source branch: `bromel777/fix_swap_path` in splash-core. Exporter and DAO successor-NFT guard are in `f8535269d7373a6ea5ddaefe4ed39c215395c409`. The exact copied CBOR was committed and pushed in `ac458834fb808f2df279c03e35f49a496c4a6d2f`.
- Frozen source bundle: `artifacts/mainnet-royalty-f853526/raw-cbor/`. The checked [release manifest](../../deployments/mainnet/royalty-scripts-f853526.json) has all 13 Plutus V2 hashes and SHA-256 sums. Production DAO public verification keys, threshold `4`, and `lpFeeIsEditable=true` are in [the admin manifest](../../deployments/mainnet/royalty-dao-v1-admins-2026-10-06.json).
- Deploy-repository copy: `/Users/aleksandr/IdeaProjects/spectrum-offchain-multiplatform/splash-testing-cardano/scripts/mainnet-royalty-f853526/`. It includes all 13 `.uplc` files, `script-hashes.txt`, `export-parameters.json`, `admin-manifest.json`, `release-manifest.json`, and `README.md`. Its release manifest says `network=mainnet`, `artifactReadiness=mainnet-ready`, and `deploymentApproved=false`.
- The deploy-repository files are **local and uncommitted**. The repository is on `develop` with other existing modifications. `splash-testing-cardano/scripts/` appears as untracked as a whole; do not stage or commit that entire directory blindly. The existing `deploy-preprod-royalty-reference-scripts.ts` was also changed locally to reject `--artifact-set=mainnet-*`.

## Verification performed

- `python3 scripts/verify_mainnet_royalty_artifacts.py artifacts/mainnet-royalty-f853526/raw-cbor --release-manifest deployments/mainnet/royalty-scripts-f853526.json` passed. It recomputes all 13 Plutus V2 hashes from CBOR and checks the SHA-256 manifest, six ordered production public keys, both DAO action-order bindings, and the nine unchanged script hashes against the earlier guarded baseline.
- The same verifier passed on the deploy-repository copy, using its local admin and release manifests. The 13 CBOR files copied there are byte-identical to the splash-core bundle.
- The Preprod publisher rejected `--artifact-set=mainnet-royalty-f853526` before any network action. Its ordinary `preprod-royalty-2026-10-05` dry run still passed.
- The new DAO V1 policy hashes are `76847f451fb7c4e1d4b93284bb5ec32439770053d86804b7b687c60f` (single) and `45613319ca4393e41499ffe47cd055718ddf2a1a1fe10c8fd879d0ee` (double). Their matching action-order hashes are `6a6af4b2aaccd011734da14b6886c95f252eddf68715172d6ce0f050` and `e25f37a05bb240778c270090f579e2ff73c3b071431a73a63ce657a5`.

## Remaining before mainnet publication

1. Obtain the remote export machine's `git rev-parse HEAD`, `git status --porcelain`, `ghc --version`, and `cabal --version`. The folder name says `f853526`, but the copied bundle itself does not attest the source revision or compiler used.
2. Run positive and negative validator evaluations against these **exact** exported bytes, including the known invalid-swap and DAO successor-NFT transitions. Byte hashes and key checks do not establish contract behavior.
3. Resolve and verify the external double-royalty withdrawal stake policy with hash `a062da72f9f3280fbed90b3e95ea943aabd7eff9c82343324a9726ad` before enabling that withdrawal flow.
4. Build and review a separate Mainnet reference-script publisher. The existing TypeScript publisher is hardcoded to Preprod; its `--artifact-set` option does not make it a Mainnet publisher. Do not broadcast before recording the intended Mainnet funding wallet, output addresses, batch plan, recovery plan, and final hash checks.
5. After publication, record and independently verify each Mainnet reference UTxO (`txHash#index`), register the required DAO stake credentials, and update the off-chain mainnet deployment configuration while preserving support for old pool versions.

No Mainnet publication transaction was made in this handoff. `mainnet-ready` describes the network identity and byte-verified artifact set; it does not mean deployment approval or a completed security review.
