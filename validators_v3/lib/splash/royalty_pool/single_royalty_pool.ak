use cardano/address.{StakeCredential}
use splash/plutus.{Asset}

pub type SingleRoyaltyPoolConfig {
  pool_nft: Asset,
  pool_x: Asset,
  pool_y: Asset,
  pool_lq: Asset,
  fee_num: Int,
  treasury_fee: Int,
  royalty_fee: Int,
  treasury_x: Int,
  treasury_y: Int,
  royalty_x: Int,
  royalty_y: Int,
  dao_policy: List<StakeCredential>,
  treasury_address: ByteArray,
  royalty_pub_key: ByteArray,
  nonce: Int,
}
