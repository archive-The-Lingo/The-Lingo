package AttrDT

type Identifier = Symbol

type UniqueIdentifier = Int

type NaturalNumber = Int

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

sealed trait Exp

sealed trait BaseType

final case class AttributeLevel(x: Option[Value])

object AttributeLevel {
  def TypeInType: AttributeLevel = AttributeLevel(None)
}

final case class AttributeSize(x: Option[Value])

object AttributeSize {
  def Diverge: AttributeSize = AttributeSize(None)

  def apply(x: Option[Value]): AttributeSize = new AttributeSize(x)

  def apply(x: NaturalNumber): AttributeSize = new AttributeSize(Some(natToValue(x)))
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

final case class ZeroV extends Value

final case class Zero extends Exp

final case class SuccV(x: Value) extends Value

final case class Succ(x: Exp) extends Exp

final case class NatV extends BaseType

final case class Nat extends Exp

def natToValue(x: NaturalNumber): Value = if (x == 0) ZeroV else SuccV(natToValue(x - 1))

final case class Atom(x: String) extends Value

final case class Quote(x: String) extends Exp

final case class UnitV extends Value

final case class Unit extends Exp

final case class TrivialV extends BaseType

final case class Trivial extends Exp

final case class ConsV(a: Value, d: Value) extends Value

final case class Cons(a: Exp, d: Exp) extends Exp

final case class SigmaV(a: TypeOrNotYet, d: Closure) extends BaseType

final case class Sigma(a: Exp, aId: Identifier, d: Exp) extends Exp

final case class Closure(id: NeuVar, body: Value) extends Value

final case class Lambda(id: Identifier, body: Exp) extends Exp

final case class PiV(domain: TypeOrNotYet, codomain: Closure) extends BaseType

final case class Pi(domain: Exp, domainId: Identifier, codomain: Exp) extends Exp

final case class Rec(id: Identifier, body: Exp) extends Exp