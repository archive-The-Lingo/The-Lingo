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

sealed trait AttributeLevel

final case class AttributeLevelKnown(x: Value) extends AttributeLevel

case object AttributeLevelTypeInType extends AttributeLevel

sealed trait AttributeSize

final case class AttributeSizeKnown(x: Value) extends AttributeSize

case object AttributeSizeFinite extends AttributeSize

sealed trait AttributeUsage

case object AttributeUsageErased extends AttributeUsage

case object AttributeUsageOnce extends AttributeUsage

case object AttributeUsageNotLimited extends AttributeUsage

final case class AttributeAssumptions(assumption: Set[TypeOrNotYet])

sealed trait AttributeDynamic

case object AttributeDynamicYes extends AttributeDynamic

case object AttributeDynamicNo extends AttributeDynamic

sealed trait AttributeDiverge

case object AttributeDivergeYes extends AttributeDiverge

case object AttributeDivergeNo extends AttributeDiverge

final case class Attrbutes(level: AttributeLevel, size: AttributeSize, usage: AttributeUsage, dynamic: AttributeDynamic, diverge: AttributeDiverge, assumptions: AttributeAssumptions)

sealed trait TypeOrNotYet extends Value

final case class Type(t: BaseType, attr: Attrbutes) extends Value with TypeOrNotYet

final case class NotYetValue(t: TypeOrNotYet, neu: Neu) extends Value with TypeOrNotYet

sealed trait Neu

final case class NeuVar(x: VarId) extends Neu

case object ZeroV extends Value

case object Zero extends Exp

final case class SuccV(x: Value) extends Value

final case class Succ(x: Exp) extends Exp

case object NatV extends BaseType

case object Nat extends Exp

def natToValue(x: NaturalNumber): Value = if (x == 0) ZeroV else SuccV(natToValue(x - 1))

final case class Atom(x: String) extends Value

final case class Quote(x: String) extends Exp

case object UnitV extends Value

case object Unit extends Exp

case object TrivialV extends BaseType

case object Trivial extends Exp

final case class ConsV(a: Value, d: Value) extends Value

final case class Cons(a: Exp, d: Exp) extends Exp

final case class SigmaV(a: TypeOrNotYet, d: Closure) extends BaseType

final case class Sigma(a: Exp, aId: Identifier, d: Exp) extends Exp

final case class Closure(id: NeuVar, body: Value) extends Value

final case class Lambda(id: Identifier, body: Exp) extends Exp

final case class PiV(domain: TypeOrNotYet, codomain: Closure) extends BaseType

final case class Pi(domain: Exp, domainId: Identifier, codomain: Exp) extends Exp

final case class Rec(id: Identifier, body: Exp) extends Exp