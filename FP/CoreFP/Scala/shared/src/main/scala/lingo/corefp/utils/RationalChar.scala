package lingo.corefp.utils

final case class RationalChar(codePoint: Long) {
  def toNat: Nat = Nat(codePoint)
}

object RationalChar {
  def apply(codePoint: Long): RationalChar = new RationalChar(codePoint)

  // todo: check if it is valid
  def apply(codePoint: Nat): RationalChar = new RationalChar(codePoint.longValue)

  def apply(x: Char): RationalChar = new RationalChar(x.toInt)
}