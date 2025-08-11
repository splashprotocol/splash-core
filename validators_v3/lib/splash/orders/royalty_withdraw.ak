use splash/plutus.{Asset}

pub type RoyaltyWithdrawConfig {
  withdraw_data: WithdrawData,
  signature: ByteArray,
  additional_bytes: ByteArray,
}

pub type WithdrawData {
  pool_nft: Asset,
  withdraw_royalty_x: Int,
  withdraw_royalty_y: Int,
  ex_fee: Int,
}

// Used to build message for signature verification
pub type WithdrawRoyaltyDataToSign {
  withdraw_data: WithdrawData,
  pool_nonce: Int,
}

pub type DoubleRoyaltyWithdrawRedeemer {
  pool_in_ix: Int,
  order_in_ix: Int,
  signer: Int,
  hash: Bool,
}

pub type RoyaltyWithdrawRedeemer {
  pool_in_ix: Int,
  order_in_ix: Int,
  hash: Bool,
}
