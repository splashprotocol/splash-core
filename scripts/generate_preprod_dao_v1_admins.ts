/**
 * Generate six disposable Preprod DAO V1 administrator identities.
 *
 * Usage:
 *   deno run --allow-read --allow-write --no-lock scripts/generate_preprod_dao_v1_admins.ts
 *
 * The private output is intentionally under .private/ (gitignored) and written
 * with mode 0600. The public manifest is safe to commit and includes both
 * verification keys and Cardano payment-key hashes. DAO V1 policies use the
 * 32-byte verification keys: the on-chain code calls verifyEd25519Signature.
 */
import * as CML from "npm:@anastasia-labs/cardano-multiplatform-lib-nodejs@6.0.2-2";
import {
  generateMnemonic,
  mnemonicToEntropy,
  validateMnemonic,
} from "npm:bip39@3.1.0";

const output = "deployments/preprod/royalty-dao-v1-mock-admins-2026-10-05.json";
const secrets = ".private/preprod-royalty-dao-v1-mock-admins-2026-10-05.json";
const hard = 0x80000000;

type PublicAdmin = {
  index: number;
  verificationKey: string;
  paymentKeyHash: string;
};

type PrivateAdmin = PublicAdmin & { mnemonic: string };

function bytesToHex(bytes: Uint8Array): string {
  return Array.from(bytes, (byte) => byte.toString(16).padStart(2, "0")).join(
    "",
  );
}

function hashToHex(hash: { to_hex(): string }): string {
  return hash.to_hex();
}

function deriveAdmin(index: number): PrivateAdmin {
  const mnemonic = generateMnemonic(256);
  if (!validateMnemonic(mnemonic)) throw new Error("BIP-39 validation failed");
  const entropy = Uint8Array.from(
    mnemonicToEntropy(mnemonic).match(/.{2}/g)!.map((byte) =>
      parseInt(byte, 16)
    ),
  );
  const root = CML.Bip32PrivateKey.from_bip39_entropy(
    entropy,
    new Uint8Array(),
  );
  const payment = root
    .derive(1852 + hard)
    .derive(1815 + hard)
    .derive(hard)
    .derive(0)
    .derive(0);
  const rawPublicKey = payment.to_public().to_raw_key();
  return {
    index,
    mnemonic,
    verificationKey: bytesToHex(rawPublicKey.to_raw_bytes()),
    paymentKeyHash: hashToHex(rawPublicKey.hash()),
  };
}

async function ensureAbsent(path: string): Promise<void> {
  try {
    await Deno.stat(path);
    throw new Error(`${path} already exists; refusing to replace key material`);
  } catch (error) {
    if (error instanceof Deno.errors.NotFound) return;
    throw error;
  }
}

await ensureAbsent(output);
await ensureAbsent(secrets);
const admins = Array.from({ length: 6 }, (_, index) => deriveAdmin(index + 1));
if (
  new Set(admins.map((admin) => admin.verificationKey)).size !== admins.length
) {
  throw new Error("generated duplicate DAO verification key");
}
if (
  admins.some((admin) =>
    admin.verificationKey.length !== 64 || admin.paymentKeyHash.length !== 56
  )
) {
  throw new Error("unexpected Cardano key encoding length");
}
const publicManifest = {
  network: "preprod",
  purpose: "Disposable mock DAO V1 administrators for Preprod only",
  threshold: 4,
  lpFeeIsEditable: true,
  daoAdminVerificationKeys: admins.map((admin) => admin.verificationKey),
  paymentKeyHashes: admins.map((admin) => admin.paymentKeyHash),
  administrators: admins.map(({ index, verificationKey, paymentKeyHash }) => ({
    index,
    verificationKey,
    paymentKeyHash,
  })),
  semantics: {
    daoAdminVerificationKeys:
      "32-byte raw Ed25519 verification keys. These exact bytes parameterize DAO V1 because the validator verifies signatures against them.",
    paymentKeyHashes:
      "28-byte Blake2b-224 Cardano payment-key hashes. Metadata only; never use as DAO V1 verification-key parameters.",
  },
};
await Deno.mkdir(".private", { recursive: true, mode: 0o700 });
await Deno.writeTextFile(
  secrets,
  `${
    JSON.stringify({ network: "preprod", administrators: admins }, null, 2)
  }\n`,
  { mode: 0o600 },
);
await Deno.chmod(secrets, 0o600);
await Deno.writeTextFile(
  output,
  `${JSON.stringify(publicManifest, null, 2)}\n`,
);
