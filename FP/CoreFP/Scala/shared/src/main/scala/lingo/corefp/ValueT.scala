package lingo.corefp

trait ValueT[T] {
  // todo: consider auto handling Component
  def apply(x: T): Value

  def unapply(x: Value): Option[T]
}
