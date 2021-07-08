package lingo.corefp

sealed trait CoreValue extends Value {
  final def core: CoreValue = this
}

final case class Atom(x: Symbol) extends CoreValue

final case class EmptyList() extends CoreValue

final case class NonEmptyList(x: Value, y: Value) extends CoreValue

final case class Tagged(x: Value, y: Value) extends CoreValue

final case class Exception(x: Value, y: Value) extends CoreValue

final case class Resource(x: Value, y: Value) extends CoreValue