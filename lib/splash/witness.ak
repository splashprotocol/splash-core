use aiken/dict.{Dict}
use aiken/transaction/credential.{Inline, ScriptCredential, StakeCredential}
use splash/plutus.{ScriptHash}

// Given the 'script_hash' checks if this script witness is presented in the withdrawals. 
pub fn contains_withdrawal(
  withdrawals: Dict<StakeCredential, Int>,
  script_hash: ScriptHash,
) -> Bool {
  let witness_scred = Inline(ScriptCredential(script_hash))
  dict.has_key(withdrawals, witness_scred)
}
