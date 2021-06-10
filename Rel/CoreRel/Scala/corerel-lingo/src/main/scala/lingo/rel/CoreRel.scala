package lingo.rel

import scala.language.implicitConversions
import scala.collection.immutable.HashMap

type Nat = Int

sealed trait Expression

// will become first-class later
sealed trait NonFirstClassGoalExpression

sealed trait NonFirstClassGoal extends NonFirstClassGoalExpression

sealed trait ValueWithHole extends Expression

sealed trait Value extends ValueWithHole

sealed trait CoreValue extends Value

case object Value {
  case object Implicits {
    implicit def lift0(x: CoreValue): Value = x

    implicit def lift1(x: Value): ValueWithHole = x

    implicit def lift2(x: ValueWithHole): Expression = x
  }
}

import Value.Implicits._

case object EmptyList extends CoreValue

// NonEmptyList - begin
sealed class NonEmptyList3(val tag: Expression, val list: Expression) extends Expression

sealed class NonEmptyList2(override val tag: ValueWithHole, override val list: ValueWithHole) extends NonEmptyList3(tag, list) with ValueWithHole

sealed class NonEmptyList1(override val tag: Value, override val list: Value) extends NonEmptyList2(tag, list) with Value

sealed class NonEmptyList0(override val tag: CoreValue, override val list: CoreValue) extends NonEmptyList1(tag, list) with CoreValue

case object NonEmptyList {
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

case object Tagged {
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

case object Exception {
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

// will become first-class later
final case class NonFirstClassRelation(args: List[Identifier], tail: Option[Identifier], x: NonFirstClassGoalExpression) extends CoreValue //TODO

// might change to Value later
type Identifier = Sym

final case class Id(x: Identifier) extends Expression with NonFirstClassGoalExpression {
  override def hashCode: Int = x.x.hashCode
}

final case class Apply(f: Expression, args: List[Expression]) extends NonFirstClassGoalExpression

final case class Hole(x: Var) extends ValueWithHole

final case class Var(x: Symbol, id: Nat) {
  override def hashCode: Int = id.hashCode
}

type Environment = HashMap[Identifier, Value]

// And - begin
sealed class And1(val x: NonFirstClassGoalExpression, val y: NonFirstClassGoalExpression) extends NonFirstClassGoalExpression

sealed class And0(override val x: NonFirstClassGoal, override val y: NonFirstClassGoal) extends And1(x, y) with NonFirstClassGoal

case object And {
  def apply(x: NonFirstClassGoal, y: NonFirstClassGoal): And0 = And0(x, y)

  def apply(x: NonFirstClassGoalExpression, y: NonFirstClassGoalExpression): And1 = And1(x, y)

  def unapply(x: And1): Some[(NonFirstClassGoalExpression, NonFirstClassGoalExpression)] = Some((x.x, x.y))

  def unapply(x: And0): Some[(NonFirstClassGoal, NonFirstClassGoal)] = Some((x.x, x.y))
}
// And - end

// Or - begin
sealed class Or1(val x: NonFirstClassGoalExpression, val y: NonFirstClassGoalExpression) extends NonFirstClassGoalExpression

sealed class Or0(override val x: NonFirstClassGoal, override val y: NonFirstClassGoal) extends Or1(x, y) with NonFirstClassGoal

case object Or {
  def apply(x: NonFirstClassGoal, y: NonFirstClassGoal): Or0 = Or0(x, y)

  def apply(x: NonFirstClassGoalExpression, y: NonFirstClassGoalExpression): Or1 = Or1(x, y)

  def unapply(x: Or1): Some[(NonFirstClassGoalExpression, NonFirstClassGoalExpression)] = Some((x.x, x.y))

  def unapply(x: Or0): Some[(NonFirstClassGoal, NonFirstClassGoal)] = Some((x.x, x.y))
}
// Or - end

// Not - begin
sealed class Not1(val x: NonFirstClassGoalExpression) extends NonFirstClassGoalExpression

sealed class Not0(override val x: NonFirstClassGoal) extends Not1(x) with NonFirstClassGoal

case object Not {
  def apply(x: NonFirstClassGoal): Not0 = Not0(x)

  def apply(x: NonFirstClassGoalExpression): Not1 = Not1(x)

  def unapply(x: Not1): Some[NonFirstClassGoalExpression] = Some(x.x)

  def unapply(x: Not0): Some[NonFirstClassGoal] = Some(x.x)
}
// Not - end

case object NonEmptyList3 {
  def unapply(x: NonEmptyList3): Some[(Expression, Expression)] = NonEmptyList.unapply(x)
}

case object Tagged3 {
  def unapply(x: Tagged3): Some[(Expression, Expression)] = Tagged.unapply(x)
}

case object Expression {
  def eval(env: Environment, x: Expression): Option[ValueWithHole] =
    x match {
      case x: ValueWithHole => Some(x)
      case Id(x) => None /*WIP*/
      case NonEmptyList3(a, b) => for {
        a <- eval(env, a)
        b <- eval(env, b)
      } yield NonEmptyList(a, b)
      case Tagged3(a, b) => for {
        a <- eval(env, a)
        b <- eval(env, b)
      } yield Tagged(a, b)
      //TODO
    }
}

case object NonFirstClassGoalExpression {
  // http://web.archive.org/web/20210610070157/https://stackoverflow.com/questions/17268334/converting-listoptiona-to-optionlista-using-map-instead-of-flatmap
  private def sequence[A](l: List[Option[A]]): Option[List[A]] = l match {
    case Nil => None
    case h :: t => h.flatMap(hh => sequence(t).map(hh :: _))
  }

  def eval(env: Environment, x: NonFirstClassGoalExpression): Option[NonFirstClassGoal] =
    x match {
      case x: NonFirstClassGoal => Some(x)
      case Id(x) => None /*WIP*/
      case Apply(f, args) => (sequence(args map (a => Expression.eval(env, a)))) flatMap (data => {
        Expression.eval(env, f) flatMap (_ match {
          case NonFirstClassRelation(args, tail, x) => if data.length < args.length then None else if data.length == args.length then None /*WIP*/
          else None /*WIP*/
          case _ => None
        })
      })
      //TODO
    }
}

final case class State(equals: HashMap[Var, ValueWithHole]) // TODO

final case class World(goals: List[NonFirstClassGoal], state: State)

final case class Universe(xs: List[World])