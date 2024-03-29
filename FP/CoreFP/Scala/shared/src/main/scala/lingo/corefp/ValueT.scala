package lingo.corefp

import izumi.reflect.Tag

trait ValueT[T] {
  def apply(x: T): Value

  def unapply(x: Value): Option[T]
}

trait ValueTAny[T] extends ValueT[T] {
  override def unapply(x: Value): Some[T]
}

trait UncachedValueT[T] extends ValueT[T] {
  protected def internal_apply(x: T): Value

  final override def apply(x: T): Value = internal_apply(x)

  protected def internal_unapply(x: Value): Option[T]

  final override def unapply(x: Value): Option[T] = internal_unapply(x)
}

trait CachedValueT[T] extends ValueT[T] {
  // https://web.archive.org/web/20210710141226/https://stackoverflow.com/questions/6983759/how-to-declare-traits-as-taking-implicit-constructor-parameters/6984823
  protected case class Helper()(implicit val ev: Tag[T])

  protected val helper: Helper
  // usage: `override val helper = Helper()`

  import helper.ev

  protected def internal_apply(x: T): Value

  final override def apply(x: T): Value = Value.addComponent(x, {
    internal_apply(x)
  })

  protected def internal_unapply(x: Value): Option[T]

  final override def unapply(x: Value): Option[T] = Value.getComponentOrAddOption(x, {
    internal_unapply(x)
  })
}

trait CachedValueTAny[T] extends ValueTAny[T] {
  protected case class Helper()(implicit val ev: Tag[T])

  protected val helper: Helper

  import helper.ev

  protected def internal_apply(x: T): Value

  final override def apply(x: T): Value = Value.addComponent(x, {
    internal_apply(x)
  })

  protected def internal_unapply(x: Value): Some[T]

  final override def unapply(x: Value): Some[T] = Some(Value.getComponentOrAdd(x, {
    internal_unapply(x).get
  }))
}
