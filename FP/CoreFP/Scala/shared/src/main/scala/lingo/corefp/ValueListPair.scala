package lingo.corefp

object ValueListPair extends ValueT[(Value, Value)] {
  override def apply(x: (Value, Value)): Value = ValueListSeq(x._1, x._2)

  override def unapply(x: Value): Option[(Value, Value)] = x match {
    case ValueListSeq(v1, v2) => Some((v1, v2))
    case _ => None
  }
}
