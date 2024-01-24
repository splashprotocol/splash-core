pub type Rational {
  num: Int,
  denom: Int,
}

pub fn rational(num: Int, denom: Int) -> Rational {
  Rational { num, denom }
}
