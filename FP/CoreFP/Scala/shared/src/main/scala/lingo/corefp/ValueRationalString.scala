package lingo.corefp

import lingo.corefp.utils.RationalString

object ValueRationalString extends CachedValueT[RationalString] {
  private val ValueListRationalChar = ValueListT(ValueRationalChar)

  override def internal_apply(x: RationalString): Value = TaggedSeq(Atoms.Tags.String, ValueListRationalChar(x.toList))

  override def internal_unapply(x: Value): Option[RationalString] = x match {
    case TaggedSeq(Atoms.Tags.String, ValueListRationalChar(xs)) => Some(RationalString(xs))
    case _ => None
  }
}
