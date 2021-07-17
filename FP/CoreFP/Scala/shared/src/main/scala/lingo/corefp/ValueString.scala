package lingo.corefp

import lingo.corefp.utils.RationalString

object ValueString extends ValueT[String] {
  override def apply(x: String): Value = ValueRationalString(RationalString(x))

  override def unapply(x: Value): Option[String] = x match {
    case ValueRationalString(s) => Some(s.valString)
    case _ => None
  }
}