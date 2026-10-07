#!/usr/bin/env python3
"""Check the exact ordinary-pool export set and Plutus V2 hashes from CBOR."""

import argparse
import hashlib
import json
import pathlib
import re


EXPECTED = {"pool", "pool-bfee", "pool-dao-policy", "pool-bfee-dao-policy"}
APPROVED_ADMIN_PKHS = (
    "68aa59a87dbdbf8f78386dc6b83e63149d8c13939a1b0cda39707f9a",
    "518a9c32deedc0b82604972692a2b7eb6c10b020d77c3c72e764b156",
    "f3a12554ca0ccd1a220b1de839a1940bc1006f08768b636fa98c66c9",
    "0ff61bd4fdda8767414642f83f1be47bd674381ef32fc0e8164aa37d",
    "d350803d45e327f8808469d5dde9e0f6fdc6e6637d85ed44cc37a12c",
    "3703634e3e34c0e7fd9a7348ad213f9272279535c73f4ec00efe00bd",
)


def verify(directory: pathlib.Path) -> None:
    parameters = json.loads((directory / "export-parameters.json").read_text())
    if (parameters.get("network") != "mainnet"
            or parameters.get("threshold") != 4
            or parameters.get("lpFeeIsEditable") is not True
            or tuple(parameters.get("paymentKeyHashes", [])) != APPROVED_ADMIN_PKHS):
        raise ValueError("ordinary DAO parameters differ from pinned production values")
    verification_keys = parameters.get("daoAdminVerificationKeys", [])
    if len(verification_keys) != len(APPROVED_ADMIN_PKHS):
        raise ValueError("ordinary DAO manifest must include six verification keys")
    for key, pkh in zip(verification_keys, APPROVED_ADMIN_PKHS):
        if not isinstance(key, str) or not re.fullmatch(r"[0-9a-f]{64}", key):
            raise ValueError("invalid administrator verification key")
        if hashlib.blake2b(bytes.fromhex(key), digest_size=28).hexdigest() != pkh:
            raise ValueError("administrator verification key/PKH mismatch")
    hashes = {}
    for line in (directory / "script-hashes.txt").read_text().splitlines():
        match = re.fullmatch(r"([a-z-]+)=([0-9a-f]{56})", line)
        if not match or match[1] in hashes:
            raise ValueError(f"invalid or duplicate hash entry: {line!r}")
        hashes[match[1]] = match[2]

    if set(hashes) != EXPECTED:
        raise ValueError(f"script set mismatch: expected {sorted(EXPECTED)}, got {sorted(hashes)}")
    files = {path.stem for path in directory.glob("*.uplc")}
    if files != EXPECTED:
        raise ValueError(f"CBOR file set mismatch: expected {sorted(EXPECTED)}, got {sorted(files)}")

    for name in sorted(EXPECTED):
        body = (directory / f"{name}.uplc").read_bytes()
        if not body:
            raise ValueError(f"empty script: {name}")
        actual = hashlib.blake2b(b"\x02" + body, digest_size=28).hexdigest()
        if actual != hashes[name]:
            raise ValueError(f"hash mismatch for {name}: listed={hashes[name]}, actual={actual}")
        if name in ("pool-dao-policy", "pool-bfee-dao-policy"):
            for pkh in APPROVED_ADMIN_PKHS:
                if body.count(bytes.fromhex(pkh)) != 1:
                    raise ValueError(f"ordinary DAO CBOR does not contain administrator PKH exactly once: {pkh}")
        print(f"{name}={actual} sha256={hashlib.sha256(body).hexdigest()} bytes={len(body)}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory", type=pathlib.Path)
    args = parser.parse_args()
    verify(args.directory)


if __name__ == "__main__":
    main()
