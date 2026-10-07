import hashlib
import pathlib
import tempfile
import unittest

from scripts.verify_mainnet_simple_pool_artifacts import verify


class SimplePoolArtifactTests(unittest.TestCase):
    def test_accepts_both_byte_verified_pool_scripts(self):
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            entries = []
            for name in ("pool", "pool-bfee"):
                body = (name + "-cbor").encode()
                (root / f"{name}.uplc").write_bytes(body)
                entries.append(f"{name}={hashlib.blake2b(bytes([2]) + body, digest_size=28).hexdigest()}")
            (root / "script-hashes.txt").write_text("\n".join(entries) + "\n")
            verify(root)

    def test_rejects_changed_script_bytes(self):
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            (root / "pool.uplc").write_bytes(b"first")
            (root / "pool-bfee.uplc").write_bytes(b"second")
            (root / "script-hashes.txt").write_text(
                "pool=" + hashlib.blake2b(b"\x02first", digest_size=28).hexdigest() + "\n"
                "pool-bfee=" + hashlib.blake2b(b"\x02second", digest_size=28).hexdigest() + "\n"
            )
            (root / "pool.uplc").write_bytes(b"tampered")
            with self.assertRaisesRegex(ValueError, "hash mismatch for pool"):
                verify(root)

    def test_rejects_incomplete_bundle(self):
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            body = b"pool-cbor"
            (root / "pool.uplc").write_bytes(body)
            (root / "script-hashes.txt").write_text(
                "pool=" + hashlib.blake2b(b"\x02" + body, digest_size=28).hexdigest() + "\n"
            )
            with self.assertRaisesRegex(ValueError, "script set mismatch"):
                verify(root)


if __name__ == "__main__":
    unittest.main()
