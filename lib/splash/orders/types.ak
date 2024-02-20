use splash/plutus.{VerificationKeyHash}

pub type OrderType =
  ByteArray

pub type OrderConf<a> {
  tpe: OrderType,
  /// Redeemer PKH.
  redeemer_reward_pkh: VerificationKeyHash,
  /// Executors permitted to execute this order.
  permitted_executors: List<VerificationKeyHash>,
  /// An extension of the config speific to order type.
  extension: a,
}
