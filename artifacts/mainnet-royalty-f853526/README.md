# Mainnet royalty script candidate

These raw Plutus V2 CBOR files were copied from
`/Users/aleksandr/mainnetScripts/artifacts/mainnet-royalty-f853526/raw-cbor`.
The mainnet export-parameter JSON and all 13 script files were present.

The byte hashes, SHA-256 values, production DAO verification keys, and DAO
action-order bindings are recorded in
`deployments/mainnet/royalty-scripts-f853526.json`. Recheck them with:

```bash
python3 scripts/verify_mainnet_royalty_artifacts.py \
  artifacts/mainnet-royalty-f853526/raw-cbor \
  --release-manifest deployments/mainnet/royalty-scripts-f853526.json
```

The directory name refers to expected source revision `f853526`. The exact
revision and toolchain used by the remote exporter were not included in the
copied files, so source-build provenance is not established by this bundle.
Successful byte verification also does not establish validator behavior for
the known exploit transitions. Treat this as a frozen candidate pending those
checks and deployment review.
