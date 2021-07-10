package lingo.corefp

import lingo.corefp.utils.Nat

object ValueNat extends CachedValueT[Nat] {
  override val helper = Helper()
  private val ValueListBoolean = ValueListT(ValueBoolean)

  override def internal_apply(x: Nat): Value = TaggedSeq(Atoms.Tags.BinaryNat, ValueListBoolean(NatUtils.nat2booleanList(x)))

  override def internal_unapply(x: Value): Option[Nat] = x match {
    case TaggedSeq(Atoms.Tags.BinaryNat, ValueListBoolean(bs)) => Some(NatUtils.booleanList2nat(bs))
    case _ => None
  }
}

// little-endian ?
private object NatUtils {
  def nat2booleanList(x: Nat): List[Boolean] =
    (0 until x.bitLength).toList.map(x.testBit)

  def booleanList2nat(xs: List[Boolean]): Nat =
    xs.zipWithIndex.foldLeft(Nat(0))((n, x) => if (x._1) n.setBit(x._2) else n)
}
