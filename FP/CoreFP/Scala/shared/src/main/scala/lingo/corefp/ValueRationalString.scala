package lingo.corefp

import lingo.corefp.utils.RationalString

object ValueRationalString extends CachedValueT[RationalString] {
  override def internal_apply(x: RationalString): Value = TaggedSeq(Atoms.Tags.String, todo())

  override def internal_unapply(x: Value): Option[RationalString] = todo()
}
