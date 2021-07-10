package lingo.corefp

import scala.reflect.runtime.universe.TypeTag

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
  private implicit val ttag: TypeTag[T] = implicitly[TypeTag[T]]

  protected def internal_apply(x: T): Value

  final override def apply(x: T): Value = Value.addComponent(x, {
    internal_apply(x)
  })

  protected def internal_unapply(x: Value): Option[T]

  final override def unapply(x: Value): Option[T] = Value.getComponentOrAddOption(x, {
    internal_unapply(x)
  })
}