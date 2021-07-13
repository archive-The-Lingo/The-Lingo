package lingo.corefp

object ValueList extends CachedValueT[List[Value]] {
  override val helper = Helper()

  override def internal_apply(xs: List[Value]): Value = xs match {
    case Nil => EmptyList()
    case head :: tail => NonEmptyList(head, apply(tail))
  }

  override def internal_unapply(x: Value): Option[List[Value]] = x match {
    case EmptyList() => Some(Nil)
    case NonEmptyList(head, tail) => unapply(tail).map(head :: _)
    case _ => None
  }
}

object ValueListSeq {
  def apply(xs: Value*): Value = ValueList(xs.toList)

  def unapplySeq(x: Value): Option[Seq[Value]] = ValueList.unapply(x)
}

final case class ListDot[T](xs: List[T], rest: T)

object ValueListDot extends CachedValueT[ListDot[Value]] {
  override protected val helper = Helper()

  override protected def internal_apply(x: ListDot[Value]): Value = x.xs match {
    case Nil => x.rest
    case head :: tail => NonEmptyList(head, apply(ListDot(tail, x.rest)))
  }

  override protected def internal_unapply(x: Value): Option[ListDot[Value]] = x match {
    case NonEmptyList(head, tail) => unapply(tail).map(tail0 => ListDot(head :: tail0.xs, tail0.rest))
    case v => Some(ListDot(Nil, v))
  }
}