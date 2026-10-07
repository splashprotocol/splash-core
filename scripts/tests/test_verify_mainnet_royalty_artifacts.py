import hashlib
import json
import pathlib
import shutil
import sys
import tempfile
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parents[1]))
import verify_mainnet_royalty_artifacts as verifier


ROOT = pathlib.Path(__file__).resolve().parents[2]
ARCHIVE = ROOT / "artifacts/archive/mainnet-royalty-deployment-2026-10-05/raw-cbor"
ADMIN_MANIFEST = ROOT / "deployments/mainnet/royalty-dao-v1-admins-2026-10-06.json"


def script_hash(body: bytes) -> str:
    return hashlib.blake2b(b"\x02" + body, digest_size=28).hexdigest()


class MainnetArtifactVerificationTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.output = pathlib.Path(self.temp.name)
        hashes = {}
        for name in verifier.EXPECTED_UNCHANGED_HASHES:
            body = (ARCHIVE / f"{name}.uplc").read_bytes()
            (self.output / f"{name}.uplc").write_bytes(body)
            hashes[name] = script_hash(body)

        keys = [bytes.fromhex(key) for key in verifier.APPROVED_ADMIN_KEYS]
        for name, marker in zip(verifier.DAO_POLICIES, (b"single", b"double")):
            body = marker + b"".join(keys)
            (self.output / f"{name}.uplc").write_bytes(body)
            hashes[name] = script_hash(body)
        for policy, order in zip(verifier.DAO_POLICIES, verifier.DAO_ORDERS):
            body = b"order" + bytes.fromhex(hashes[policy])
            (self.output / f"{order}.uplc").write_bytes(body)
            hashes[order] = script_hash(body)

        (self.output / "script-hashes.txt").write_text(
            "".join(f"{name}={hashes[name]}\n" for name in verifier.EXPECTED)
        )
        shutil.copyfile(ADMIN_MANIFEST, self.output / "export-parameters.json")

    def test_complete_consistent_bundle_is_accepted(self):
        verifier.verify(self.output, ADMIN_MANIFEST)

    def test_wrong_admin_keys_are_rejected(self):
        parameters = json.loads((self.output / "export-parameters.json").read_text())
        parameters["daoAdminVerificationKeys"][0] = "00" * 32
        (self.output / "export-parameters.json").write_text(json.dumps(parameters))
        with self.assertRaisesRegex(ValueError, "export parameters"):
            verifier.verify(self.output, ADMIN_MANIFEST)

    def test_stale_order_binding_is_rejected(self):
        order = verifier.DAO_ORDERS[0]
        body = b"order" + bytes.fromhex("00" * 28)
        (self.output / f"{order}.uplc").write_bytes(body)
        hashes_path = self.output / "script-hashes.txt"
        hashes_path.write_text(
            hashes_path.read_text().replace(
                next(line for line in hashes_path.read_text().splitlines() if line.startswith(order + "=")),
                f"{order}={script_hash(body)}",
            )
        )
        with self.assertRaisesRegex(ValueError, "does not contain its final DAO policy hash"):
            verifier.verify(self.output, ADMIN_MANIFEST)

    def test_release_manifest_rejects_wrong_sha256(self):
        hashes = verifier.read_hashes(self.output / "script-hashes.txt")
        release_path = self.output / "release.json"
        release_path.write_text(json.dumps({
            "network": "mainnet",
            "scriptCount": len(verifier.EXPECTED),
            "scripts": {
                name: {
                    "hash": hashes[name],
                    "sha256": hashlib.sha256((self.output / f"{name}.uplc").read_bytes()).hexdigest(),
                }
                for name in verifier.EXPECTED
            },
        }))
        verifier.verify_release_manifest(self.output, release_path)
        release = json.loads(release_path.read_text())
        release["scripts"]["royalty-pool"]["sha256"] = "00" * 32
        release_path.write_text(json.dumps(release))
        with self.assertRaisesRegex(ValueError, "SHA-256 mismatch"):
            verifier.verify_release_manifest(self.output, release_path)


if __name__ == "__main__":
    unittest.main()
