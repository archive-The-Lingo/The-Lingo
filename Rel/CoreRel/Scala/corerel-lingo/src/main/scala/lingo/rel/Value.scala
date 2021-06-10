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
  }
}

import Value.Implicits._

final case object EmptyList extends CoreValue

// NonEmptyList - begin
sealed class NonEmptyList3(val tag: Expression, val list: Expression) extends Expression

sealed class NonEmptyList2(override val tag: ValueWithHole, override val list: ValueWithHole) extends NonEmptyList3(tag, list) with ValueWithHole

sealed class NonEmptyList1(override val tag: Value, override val list: Value) extends NonEmptyList2(tag, list) with Value

sealed class NonEmptyList0(override val tag: CoreValue, override val list: CoreValue) extends NonEmptyList1(tag, list) with CoreValue

final object NonEmptyList {
  def apply(tag: CoreValue, list: CoreValue): NonEmptyList0 = NonEmptyList0(tag, list)

  def apply(tag: Value, list: Value): NonEmptyList1 = NonEmptyList1(tag, list)

  def apply(tag: ValueWithHole, list: ValueWithHole): NonEmptyList2 = NonEmptyList2(tag, list)

  def apply(tag: Expression, list: Expression): NonEmptyList3 = NonEmptyList3(tag, list)

  def unapply(x: NonEmptyList3): Some[(Expression, Expression)] = Some((x.tag, x.list))

  def unapply(x: NonEmptyList2): Some[(ValueWithHole, ValueWithHole)] = Some((x.tag, x.list))

  def unapply(x: NonEmptyList1): Some[(Value, Value)] = Some((x.tag, x.list))

  def unapply(x: NonEmptyList0): Some[(CoreValue, CoreValue)] = Some((x.tag, x.list))
}
// NonEmptyList - end

final case class Sym(x: Symbol) extends CoreValue

// Tagged - begin
sealed class Tagged3(val tag: Expression, val list: Expression) extends Expression

sealed class Tagged2(override val tag: ValueWithHole, override val list: ValueWithHole) extends Tagged3(tag, list) with ValueWithHole

sealed class Tagged1(override val tag: Value, override val list: Value) extends Tagged2(tag, list) with Value

sealed class Tagged0(override val tag: CoreValue, override val list: CoreValue) extends Tagged1(tag, list) with CoreValue

final object Tagged {
  def apply(tag: CoreValue, list: CoreValue): Tagged0 = Tagged0(tag, list)

  def apply(tag: Value, list: Value): Tagged1 = Tagged1(tag, list)

  def apply(tag: ValueWithHole, list: ValueWithHole): Tagged2 = Tagged2(tag, list)

  def apply(tag: Expression, list: Expression): Tagged3 = Tagged3(tag, list)

  def unapply(x: Tagged3): Some[(Expression, Expression)] = Some((x.tag, x.list))

  def unapply(x: Tagged2): Some[(ValueWithHole, ValueWithHole)] = Some((x.tag, x.list))

  def unapply(x: Tagged1): Some[(Value, Value)] = Some((x.tag, x.list))

  def unapply(x: Tagged0): Some[(CoreValue, CoreValue)] = Some((x.tag, x.list))
}
// Tagged - end

// Exception - begin
sealed class Exception3(val tag: Expression, val list: Expression) extends Expression

sealed class Exception2(override val tag: ValueWithHole, override val list: ValueWithHole) extends Exception3(tag, list) with ValueWithHole

sealed class Exception1(override val tag: Value, override val list: Value) extends Exception2(tag, list) with Value

sealed class Exception0(override val tag: CoreValue, override val list: CoreValue) extends Exception1(tag, list) with CoreValue

final object Exception {
  def apply(tag: CoreValue, list: CoreValue): Exception0 = Exception0(tag, list)

  def apply(tag: Value, list: Value): Exception1 = Exception1(tag, list)

  def apply(tag: ValueWithHole, list: ValueWithHole): Exception2 = Exception2(tag, list)

  def apply(tag: Expression, list: Expression): Exception3 = Exception3(tag, list)

  def unapply(x: Exception3): Some[(Expression, Expression)] = Some((x.tag, x.list))

  def unapply(x: Exception2): Some[(ValueWithHole, ValueWithHole)] = Some((x.tag, x.list))

  def unapply(x: Exception1): Some[(Value, Value)] = Some((x.tag, x.list))

  def unapply(x: Exception0): Some[(CoreValue, CoreValue)] = Some((x.tag, x.list))
}
// Exception - end

final case class NonFirstClassRelation() extends CoreValue //TODO

final case class Id(x: Value) extends Expression

final case class Apply(f: Expression, args: List[Expression]) extends Expression

final case class Hole(x: Var) extends ValueWithHole

final case class Var(x: Symbol, id: Identifier)