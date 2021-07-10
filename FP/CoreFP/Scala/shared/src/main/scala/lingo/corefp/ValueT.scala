package lingo.corefp

import izumi.reflect.Tag

trait ValueT[T] {
  // todo: consider auto handling Component
  def apply(x: T): Value

  def unapply(x: Value): Option[T]
}

trait UncachedValueT[T] extends ValueT[T] {
  protected def internal_apply(x: T): Value

  final override def apply(x: T): Value = internal_apply(x)

  protected def internal_unapply(x: Value): Option[T]

  final override def unapply(x: Value): Option[T] = internal_unapply(x)
}

trait CachedValueT[T] extends ValueT[T] {
  private implicit val rtag: Tag[T] = implicitly[Tag[T]]

  protected def internal_apply(x: T): Value

  final override def apply(x: T): Value = Value.addComponent(x, {
    internal_apply(x)
  })

  protected def internal_unapply(x: Value): Option[T]

  final override def unapply(x: Value): Option[T] = Value.getComponentOrAddOption(x, {
    internal_unapply(x)
  })
}


private[corefp] final case class ValueListT[T](valueT: ValueT[T])(implicit ev: Tag[T]) extends ValueT[List[T]] {
  private def traverse[U](xs: List[Option[U]]): Option[List[U]] = xs match {
    case Nil => Some(Nil)
    case Some(head) :: tail => traverse(tail).map(head :: _)
    case None :: _ => None
  }

  override def apply(xs: List[T]): Value = ValueList(xs.map(valueT.apply))

  override def unapply(x: Value): Option[List[T]] = ValueList.unapply(x).flatMap(xs => traverse(xs.map(valueT.unapply)))
}
