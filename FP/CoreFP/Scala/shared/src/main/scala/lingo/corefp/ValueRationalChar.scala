package lingo.corefp

import lingo.corefp.utils.RationalChar

object ValueRationalChar extends CachedValueT[RationalChar] {
  override val helper = Helper()
  def internal_apply(x: RationalChar): Value = TaggedSeq(Atoms.Tags.Char, ValueNat(x.toNat))

  def internal_unapply(x: Value): Option[RationalChar] = x match {
    case TaggedSeq(Atoms.Tags.Char, ValueNat(n)) => Some(RationalChar(n))
    case _ => None
  }
}
