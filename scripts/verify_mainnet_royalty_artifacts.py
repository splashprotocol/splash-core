#!/usr/bin/env python3
"""Verify a complete production royalty CBOR export before it is published."""

import argparse
import hashlib
import json
import pathlib
import re
import sys


EXPECTED = (
    "royalty-pool",
    "double-royalty-pool",
    "royalty-deposit",
    "royalty-redeem",
    "double-royalty-deposit",
    "double-royalty-redeem",
    "royalty-dao-v1-policy",
    "double-royalty-dao-v1-policy",
    "royalty-dao-v1-action-order",
    "double-royalty-dao-v1-action-order",
    "royalty-withdraw-order",
    "double-royalty-withdraw-order",
    "royalty-withdraw-pool-policy",
)
DAO_POLICIES = ("royalty-dao-v1-policy", "double-royalty-dao-v1-policy")
DAO_ORDERS = ("royalty-dao-v1-action-order", "double-royalty-dao-v1-action-order")
APPROVED_ADMIN_KEYS = (
    "0bb1d2db22f9b641f0afe8d8a398279cb778d8f86167500f7e63ebbdc35b4d69",
    "4e8221615500dbf6737b02992610ffeed82da6826dc3d9729febf1d32d766615",
    "ae536160ccec4f078982396125773d509072397e35ed6fab7af2a762ca147318",
    "04e3a257bcb0306c27e796bc16d1b7bde8f2306dc1d6aa344f6043ef48bd7fd8",
    "83d3aa4ccd1c72ff7a27c032394f44b699778b6050290e37fd82bc295f5caf18",
    "a7d30e99673c57638bdb65b5a0554ddee3135131940a41bbd3534b0d4c709506",
)
EXPECTED_UNCHANGED_HASHES = {
    "royalty-pool": "72ed0eaa3594bfadd006992ed574dee50bbced070955c0c296fb8f8e",
    "double-royalty-pool": "ca6c752e43e47c6287d137b6d3f0f0d22a5c23b61910e3abcdcf81b2",
    "royalty-deposit": "5a04e05f6fd8b6e03c19619417abfec145b44f237d58bed30dc9f694",
    "royalty-redeem": "fceb645fdc06550d62065bc0f95318e8dcd71e6a8c13c05f313b246d",
    "double-royalty-deposit": "821a8ed3a90050b9a7f17c7ccdb94d4a79e3e425cff22d7bc0771d0b",
    "double-royalty-redeem": "16635db270fd379d0d3467dc09b6d0aa613e9ffe6fdf5a02fa5a2154",
    "royalty-withdraw-order": "92c094b90cf3637a96a13e9bc9a04ce8bb7e48c7ed0b5d1cc5ca7332",
    "double-royalty-withdraw-order": "6710f3002759d028a90a08e1538b25d4eaf48c72d88157f71d68dcc0",
    "royalty-withdraw-pool-policy": "e0de1016c69f04483037d4cb6cc85bb6d42bc75b80774bc5bc48eb2f",
}
FORBIDDEN_DAO_HASHES = {
    "56e45b69e269ac0cdae96f52c28b6ae2f1daf1b2d5777b212a55c3ee",
    "591b89ca42216543df1d911df3eef1145bff5a9d15d6eff8f697810f",
    "90052675bc42599a1ad33ec3b547c1890fc6f68da05212dee86c7377",
    "f687c733f2a878f63d8f61d8f3a57585d5a24087843d5eca559753b2",
}
FORBIDDEN_ORDER_HASHES = {
    "2073f4c84bd09c2d8e11018c6f106fd2e00a643e118d90167d17a1d9",
    "5183b498297de742b6aba40c4ad5de442dea5e6237872c4cce734cfe",
    "95d1b3118dd7fd24f3e986f7952ee4dc3fd373ad8a70e0e4a6115c71",
}


def fail(message: str) -> None:
    raise ValueError(message)


def read_hashes(path: pathlib.Path) -> dict[str, str]:
    hashes: dict[str, str] = {}
    for line in path.read_text().splitlines():
        parts = line.split("=", 1)
        if len(parts) != 2 or not re.fullmatch(r"[0-9a-f]{56}", parts[1]):
            fail(f"invalid script-hashes.txt line: {line!r}")
        name, script_hash = parts
        if name in hashes:
            fail(f"duplicate script name: {name}")
        hashes[name] = script_hash
    return hashes


def verify(directory: pathlib.Path, admin_manifest: pathlib.Path) -> None:
    admins = json.loads(admin_manifest.read_text())
    if admins.get("network") != "mainnet" or admins.get("threshold") != 4 or admins.get("lpFeeIsEditable") is not True:
        fail("production administrator parameters do not match the approved manifest")
    keys = admins.get("daoAdminVerificationKeys", [])
    pkhs = admins.get("paymentKeyHashes", [])
    if tuple(keys) != APPROVED_ADMIN_KEYS or len(pkhs) != 6:
        fail("production verification keys differ from the approved ordered set")
    for key, pkh in zip(keys, pkhs):
        if not re.fullmatch(r"[0-9a-f]{64}", key):
            fail("invalid production verification key")
        if hashlib.blake2b(bytes.fromhex(key), digest_size=28).hexdigest() != pkh:
            fail("production verification key/PKH mismatch")

    hashes = read_hashes(directory / "script-hashes.txt")
    if set(hashes) != set(EXPECTED):
        fail(f"script set mismatch: missing={sorted(set(EXPECTED) - set(hashes))}, extra={sorted(set(hashes) - set(EXPECTED))}")
    files = {path.stem for path in directory.glob("*.uplc")}
    if files != set(EXPECTED):
        fail(f"CBOR file set mismatch: missing={sorted(set(EXPECTED) - files)}, extra={sorted(files - set(EXPECTED))}")
    export_input = json.loads((directory / "export-parameters.json").read_text())
    if (
        export_input.get("network") != "mainnet"
        or tuple(export_input.get("daoAdminVerificationKeys", [])) != APPROVED_ADMIN_KEYS
        or export_input.get("threshold") != 4
        or export_input.get("lpFeeIsEditable") is not True
    ):
        fail("export parameters do not match the pinned mainnet parameters")

    bodies: dict[str, bytes] = {}
    for name in EXPECTED:
        body = (directory / f"{name}.uplc").read_bytes()
        if not body:
            fail(f"empty script: {name}")
        calculated = hashlib.blake2b(b"\x02" + body, digest_size=28).hexdigest()
        if calculated != hashes[name]:
            fail(f"hash mismatch for {name}: manifest={hashes[name]}, calculated={calculated}")
        bodies[name] = body

    for name, expected_hash in EXPECTED_UNCHANGED_HASHES.items():
        if hashes[name] != expected_hash:
            fail(f"{name} differs from the reviewed guarded release baseline: {hashes[name]}")

    for name in DAO_POLICIES:
        if hashes[name] in FORBIDDEN_DAO_HASHES:
            fail(f"stale or Preprod DAO policy: {name}={hashes[name]}")
        for key in keys:
            if bodies[name].count(bytes.fromhex(key)) != 1:
                fail(f"{name} does not contain the approved production key set exactly once")
    for policy, order in zip(DAO_POLICIES, DAO_ORDERS):
        if hashes[order] in FORBIDDEN_ORDER_HASHES:
            fail(f"stale or Preprod DAO order: {order}={hashes[order]}")
        if bytes.fromhex(hashes[policy]) not in bodies[order]:
            fail(f"{order} does not contain its final DAO policy hash")
    print("Verified 13 mainnet royalty CBOR files, policy keys, DAO order bindings, and hashes.")
    for name in EXPECTED:
        print(f"{name}={hashes[name]}")


def verify_release_manifest(directory: pathlib.Path, release_manifest: pathlib.Path) -> None:
    release = json.loads(release_manifest.read_text())
    if release.get("network") != "mainnet" or release.get("scriptCount") != len(EXPECTED):
        fail("release manifest network or script count mismatch")
    scripts = release.get("scripts", {})
    if set(scripts) != set(EXPECTED):
        fail("release manifest script set mismatch")
    hashes = read_hashes(directory / "script-hashes.txt")
    for name in EXPECTED:
        body = (directory / f"{name}.uplc").read_bytes()
        if scripts[name].get("hash") != hashes[name]:
            fail(f"release manifest script hash mismatch: {name}")
        sha256 = hashlib.sha256(body).hexdigest()
        if scripts[name].get("sha256") != sha256:
            fail(f"release manifest SHA-256 mismatch: {name}")
    print(f"Verified release manifest: {release_manifest}")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("artifact_dir", type=pathlib.Path)
    parser.add_argument(
        "--admin-manifest",
        type=pathlib.Path,
        default=pathlib.Path(__file__).resolve().parents[1]
        / "deployments/mainnet/royalty-dao-v1-admins-2026-10-06.json",
    )
    parser.add_argument("--release-manifest", type=pathlib.Path)
    arguments = parser.parse_args()
    try:
        verify(arguments.artifact_dir, arguments.admin_manifest)
        if arguments.release_manifest:
            verify_release_manifest(arguments.artifact_dir, arguments.release_manifest)
    except (OSError, ValueError, json.JSONDecodeError) as error:
        print(f"REJECTED: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
