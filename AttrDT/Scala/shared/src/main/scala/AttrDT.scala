package AttrDT

type Identifier = Symbol

type UniqueIdentifier = Int

type Nat = Int

object UniqueIdentifier {
  private var count: UniqueIdentifier = 0

  def gen: UniqueIdentifier = this.synchronized {
    val result = count
    count = count + 1
    result
  }
}

final case class VarId(id: Identifier, uid: UniqueIdentifier)

sealed trait Value

sealed trait BaseType

final case class AttributeLevel(x: Option[Value])

object AttributeLevel {
  def TypeInType: AttributeLevel = AttributeLevel(None)
}

final case class AttributeSize(x: Option[Value])

object AttributeSize {
  def Diverge: AttributeSize = AttributeSize(None)

  def apply(x: Option[Value]): AttributeSize = new AttributeSize(x)

  def apply(x: Nat): AttributeSize = new AttributeSize(Some(natToValue(x)))
}

sealed trait AttributeUsage

case object AttributeUsageErased extends AttributeUsage

case object AttributeUsageOnce extends AttributeUsage

case object AttributeUsageNotLimited extends AttributeUsage

final case class AttributeAssumptions(assumption: Set[TypeOrNotYet])

final case class Attrbutes(level: AttributeLevel, size: Option[AttributeSize], usage: AttributeUsage, dynamic: Boolean, assumptions: AttributeAssumptions)

sealed trait TypeOrNotYet extends Value

final case class Type(t: BaseType, attr: Attrbutes) extends Value with TypeOrNotYet

final case class NotYetValue(t: TypeOrNotYet, neu: Neu) extends Value with TypeOrNotYet

sealed trait Neu

final case class NeuVar(x: VarId) extends Neu

final case class Zero extends Value

final case class Succ(x: Value) extends Value

def natToValue(x: Nat): Value = if (x == 0) Zero else Succ(natToValue(x - 1))

final case class Atom(x: Symbol) extends Value

final case class Unit extends Value

final case class Trivial extends BaseType

final case class Cons(a: Value, d: Value) extends Value

final case class Closure(id: NeuVar, body: Value) extends Value