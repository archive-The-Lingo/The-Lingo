package lingo.corefp

import izumi.reflect.Tag

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

private[corefp] object ValueListT {
  def traverse[U](xs: List[Option[U]]): Option[List[U]] = xs match {
    case Nil => Some(Nil)
    case Some(head) :: tail => traverse(tail).map(head :: _)
    case None :: _ => None
  }

}

private[corefp] final case class ValueListT[T](valueT: ValueT[T])(implicit ev: Tag[T]) extends ValueT[List[T]] {

  import ValueListT.traverse

  override def apply(xs: List[T]): Value = ValueList(xs.map(valueT.apply))

  override def unapply(x: Value): Option[List[T]] = ValueList.unapply(x).flatMap(xs => traverse(xs.map(valueT.unapply)))
}

final case class ListDot[T, U](xs: List[T], rest: U)

object ValueListDot extends CachedValueTAny[ListDot[Value, Value]] {
  override protected val helper = Helper()

  override protected def internal_apply(x: ListDot[Value, Value]): Value = x.xs match {
    case Nil => x.rest
    case head :: tail => NonEmptyList(head, apply(ListDot(tail, x.rest)))
  }

  override protected def internal_unapply(x: Value): Some[ListDot[Value, Value]] = x match {
    case NonEmptyList(head, tail) => {
      val Some(tail0) = unapply(tail)
      Some(ListDot(head :: tail0.xs, tail0.rest))
    }
    case v => Some(ListDot(Nil, v))
  }
}

private[corefp] final case class ValueListDotTU[T, U](valueT: ValueT[T], valueU: ValueT[U])(implicit evT: Tag[T], evU: Tag[U]) extends ValueT[ListDot[T, U]] {

  import ValueListT.traverse

  override def unapply(x: Value): Option[ListDot[T, U]] = x match {
    case ValueListDot(ListDot(xs, valueU(rest))) => traverse(xs.map(valueT.unapply)).map(ListDot(_, rest))
    case _ => None
  }

  override def apply(x: ListDot[T, U]): Value = ValueListDot(ListDot(x.xs.map(valueT(_)), valueU(x.rest)))
}