package lingo.corefp

import lingo.corefp.utils.RationalString

object ValueString extends CachedValueT[String] {
  override def internal_apply(x: String): Value = ValueRationalString(RationalString(x))

  override def internal_unapply(x: Value): Option[String] = ValueRationalString.unapply(x).map(_.valString)
}
