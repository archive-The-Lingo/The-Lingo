package lingo.corefp

import lingo.corefp.utils.Nat

object ValueNat {
  def apply(x: Nat): Value = todo()

  def unapply(x: Value): Option[Nat] = Value.getComponentOrAddOption(x, {
    x match {
      case Tagged(Atoms.Tags.BinaryNat, ValueList(_)) => todo()
      case _ => None
    }
  })
}

private object NatUtils {
  def nat2booleanList(x: Nat): List[Boolean] =
    (0 until x.bitLength).toList.map(x.testBit)

  def booleanList2nat(xs: List[Boolean]): Nat =
    xs.zipWithIndex.foldLeft(Nat(0))((n, x) => if (x._1) n.setBit(x._2) else n)
}