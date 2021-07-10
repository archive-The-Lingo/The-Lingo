package lingo.corefp.utils

import java.lang.Character

final case class RationalChar(codePoint: Int) {
  def toNat: Nat = Nat(codePoint)

  def valString: String = Character.toString(codePoint)
}

object RationalChar {
  def apply(codePoint: Int): RationalChar = new RationalChar(codePoint)

  def apply(codePoint: Long): RationalChar = new RationalChar(codePoint.toInt)

  // todo: check if it is valid
  def apply(codePoint: Nat): RationalChar = new RationalChar(codePoint.intValue)

  def apply(x: Char): RationalChar = new RationalChar(x.toInt)
}