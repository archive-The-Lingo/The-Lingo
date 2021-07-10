package lingo.corefp

object ValueList extends CachedValueT[List[Value]] {
  override def internal_apply(xs: List[Value]): Value = xs match {
    case Nil => EmptyList
    case head :: tail => NonEmptyList(head, apply(tail))
  }

  override def internal_unapply(x: Value): Option[List[Value]] = x match {
    case EmptyList => Some(Nil)
    case NonEmptyList(head, tail) => unapply(tail).map(head :: _)
    case _ => None
  }
}

object ValueListSeq {
  def unapplySeq(x: Value): Option[Seq[Value]] = ValueList.unapply(x)
}