import hashlib
import pathlib
import tempfile
import unittest
import json

from scripts.verify_mainnet_simple_pool_artifacts import APPROVED_ADMIN_PKHS, verify


ADMIN_MANIFEST = pathlib.Path(__file__).resolve().parents[2] / "deployments/mainnet/royalty-dao-v1-admins-2026-10-06.json"


class SimplePoolArtifactTests(unittest.TestCase):
    def make_bundle(self, root):
        names = ("pool", "pool-bfee", "pool-dao-policy")
        entries = []
        for name in names:
            body = (name + "-cbor").encode()
            if name == "pool-dao-policy":
                body += b"".join(bytes.fromhex(pkh) for pkh in APPROVED_ADMIN_PKHS)
            (root / f"{name}.uplc").write_bytes(body)
            entries.append(f"{name}={hashlib.blake2b(bytes([2]) + body, digest_size=28).hexdigest()}")
        (root / "script-hashes.txt").write_text("\n".join(entries) + "\n")
        (root / "export-parameters.json").write_text(ADMIN_MANIFEST.read_text())

    def test_accepts_byte_verified_pool_and_dao_scripts(self):
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            self.make_bundle(root)
            verify(root)

    def test_rejects_changed_script_bytes(self):
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            self.make_bundle(root)
            (root / "pool.uplc").write_bytes(b"tampered")
            with self.assertRaisesRegex(ValueError, "hash mismatch for pool"):
                verify(root)

    def test_rejects_incomplete_bundle(self):
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            self.make_bundle(root)
            (root / "script-hashes.txt").write_text(
                (root / "script-hashes.txt").read_text().replace("pool-dao-policy=", "missing-policy=")
            )
            with self.assertRaisesRegex(ValueError, "script set mismatch"):
                verify(root)

    def test_rejects_wrong_dao_administrator_set(self):
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            self.make_bundle(root)
            parameters = json.loads((root / "export-parameters.json").read_text())
            parameters["paymentKeyHashes"][0] = "00" * 28
            (root / "export-parameters.json").write_text(json.dumps(parameters))
            with self.assertRaisesRegex(ValueError, "DAO parameters differ"):
                verify(root)

    def test_rejects_mismatched_verification_key_and_pkh(self):
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            self.make_bundle(root)
            parameters = json.loads((root / "export-parameters.json").read_text())
            parameters["daoAdminVerificationKeys"][0] = "00" * 32
            (root / "export-parameters.json").write_text(json.dumps(parameters))
            with self.assertRaisesRegex(ValueError, "verification key/PKH mismatch"):
                verify(root)

    def test_rejects_policy_without_pinned_admin_hash(self):
        with tempfile.TemporaryDirectory() as directory:
            root = pathlib.Path(directory)
            self.make_bundle(root)
            policy_path = root / "pool-dao-policy.uplc"
            body = policy_path.read_bytes().replace(bytes.fromhex(APPROVED_ADMIN_PKHS[0]), b"x" * 28)
            policy_path.write_bytes(body)
            lines = (root / "script-hashes.txt").read_text().splitlines()
            lines = [
                line if not line.startswith("pool-dao-policy=") else
                "pool-dao-policy=" + hashlib.blake2b(b"\x02" + body, digest_size=28).hexdigest()
                for line in lines
            ]
            (root / "script-hashes.txt").write_text("\n".join(lines) + "\n")
            with self.assertRaisesRegex(ValueError, "administrator PKH"):
                verify(root)


if __name__ == "__main__":
    unittest.main()
