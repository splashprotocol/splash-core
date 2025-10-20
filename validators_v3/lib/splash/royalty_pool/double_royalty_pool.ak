use cardano/address.{StakeCredential}
use splash/plutus.{Asset}

pub type DoubleRoyaltyPoolConfig {
  pool_nft: Asset,
  pool_x: Asset,
  pool_y: Asset,
  pool_lq: Asset,
  fee_num: Int,
  treasury_fee: Int,
  first_royalty_fee: Int,
  second_royalty_fee: Int,
  treasury_x: Int,
  treasury_y: Int,
  first_royalty_x: Int,
  first_royalty_y: Int,
  second_royalty_x: Int,
  second_royalty_y: Int,
  dao_policy: List<StakeCredential>,
  treasury_address: ByteArray,
  first_royalty_pub_key: ByteArray,
  second_royalty_pub_key: ByteArray,
  nonce: Int,
}
