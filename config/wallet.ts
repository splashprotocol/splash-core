import {fromHex, Lucid} from "npm:@lucid-evolution/lucid";
import * as CML from 'npm:@anastasia-labs/cardano-multiplatform-lib-nodejs';

export async function setupWallet(lucid: Lucid) {
  const seed = await Deno.readTextFile('./seed.txt');
  return lucid.selectWallet.fromSeed(seed);
}

export async function getPrivateKey() {
  const key = await Deno.readTextFile('./privateKey.txt');
  return CML.Bip32PrivateKey.from_bech32(key);
}

