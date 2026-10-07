#!/usr/bin/env python3
"""Check the exact ordinary-pool export set and Plutus V2 hashes from CBOR."""

import argparse
import hashlib
import pathlib
import re


EXPECTED = {"pool", "pool-bfee"}


def verify(directory: pathlib.Path) -> None:
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
        print(f"{name}={actual} sha256={hashlib.sha256(body).hexdigest()} bytes={len(body)}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory", type=pathlib.Path)
    args = parser.parse_args()
    verify(args.directory)


if __name__ == "__main__":
    main()
