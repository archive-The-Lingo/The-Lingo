package lingo.rel

import scala.language.implicitConversions

type Nat = Int

sealed trait Expression

sealed trait ValueWithHole extends Expression

type Identifier = Value

sealed trait Value extends ValueWithHole

sealed trait CoreValue extends Value

final case object Value {
  final case object Implicits {
    implicit def lift0(x: CoreValue): Value = x

    implicit def lift1(x: Value): ValueWithHole = x

    implicit def lift2(x: ValueWithHole): Expression = x

    implicit def Tagged_lift0(x: Tagged0): Tagged1 = Tagged1(x.tag, x.list)

    implicit def Tagged_lift1(x: Tagged1): Tagged2 = Tagged2(x.tag, x.list)

    implicit def Tagged_lift2(x: Tagged2): Tagged3 = Tagged3(x.tag, x.list)
  }
}

import Value.Implicits._

final case object EmptyList extends CoreValue

final case class NonEmptyList(x: CoreValue, xs: CoreValue) extends CoreValue

final case class Sym(x: Symbol) extends CoreValue

final case class Tagged0(tag: CoreValue, list: CoreValue) extends CoreValue

final case class Tagged1(tag: Value, list: Value) extends Value

final case class Tagged2(tag: ValueWithHole, list: ValueWithHole) extends ValueWithHole

final case class Tagged3(tag: Expression, list: Expression) extends Expression

final case object Tagged {
  def apply(tag: CoreValue, list: CoreValue): CoreValue = Tagged0(tag, list)

  def apply(tag: Value, list: Value): Value = Tagged1(tag, list)

  def apply(tag: ValueWithHole, list: ValueWithHole): ValueWithHole = Tagged2(tag, list)

  def apply(tag: Expression, list: Expression): Expression = Tagged3(tag, list)
}

final case class Exception(tag: CoreValue, list: CoreValue) extends CoreValue

final case class NonFirstClassRelation() extends CoreValue //TODO

final case class Id(x: Value) extends Expression

final case class Apply(f: Expression, args: List[Expression]) extends Expression

final case class Hole(x: Var) extends ValueWithHole

final case class Var(x: Symbol, id: Identifier)