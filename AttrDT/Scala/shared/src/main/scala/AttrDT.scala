package AttrDT

import scala.collection.immutable.HashMap

type Identifier = Symbol

type UniqueIdentifier = Int

type NaturalNumber = Int

final case class Error(x: String) extends Exception(x)

object UniqueIdentifier {
  private var count: UniqueIdentifier = 0

  def gen: UniqueIdentifier = this.synchronized {
    val result = count
    count = count + 1
    result
  }
}

final case class VarId(id: Identifier, uid: UniqueIdentifier)

object VarId {
  def gen(id: Identifier): VarId = VarId(id, UniqueIdentifier.gen)
}

final case class Env(inner: HashMap[Identifier, (Type, Value)]) {
  def updated(id: Identifier, t: Type, v: Value): Env = Env(inner.updated(id, (t, v)))

  def get(id: Identifier): Option[(Type, Value)] = inner.get(id)

  def getType(id: Identifier): Option[Type] = inner.get(id).map(_._1)

  def getValue(id: Identifier): Option[Value] = inner.get(id).map(_._2)
}

object Env {
  val Empty: Env = Env(HashMap())
}

sealed trait Value {
  def readback(t: Type): Exp = ???
}

sealed trait Exp {
  def eval(env: Env): Value = ???

  def check(env: Env, t: Type): Boolean = ???

  def infer(env: Env): Option[Type] = ???
}

sealed trait BaseType

sealed trait Attribute

sealed trait AttributeLevel extends Attribute

final case class AttributeLevelKnown(x: Value) extends AttributeLevel

case object AttributeLevelTypeInType extends AttributeLevel

sealed trait AttributeSize extends Attribute

final case class AttributeSizeKnown(x: Value) extends AttributeSize

case object AttributeSizeFinite extends AttributeSize

sealed trait AttributeUsage extends Attribute

case object AttributeUsageErased extends AttributeUsage

case object AttributeUsageOnce extends AttributeUsage

case object AttributeUsageNotLimited extends AttributeUsage

final case class AttributeAssumptions(assumption: Set[Type]) extends Attribute

sealed trait AttributeDynamic extends Attribute

case object AttributeDynamicYes extends AttributeDynamic

case object AttributeDynamicNo extends AttributeDynamic

sealed trait AttributeDiverge extends Attribute

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

final case class Car(x: Exp) extends Exp

final case class NeuCar(x: Neu) extends Neu

final case class Cdr(x: Exp) extends Exp

final case class NeuCdr(x: Neu) extends Neu

final case class Apply(f: Exp, xs: List[Exp]) extends Exp

final case class NeuApply(f: Neu, xs: List[Value]) extends Neu

case object Universe extends Exp

case object UniverseV extends BaseType

final case class Eq(t: Exp, x: Exp, y: Exp) extends Exp

final case class EqV(t: Type, x: Value, y: Value) extends BaseType

case object Same extends Exp

case object SameV extends Value

final case class Attributed(attr: List[Attribute], t: Exp) extends Exp

case object Absurd extends Exp

case object AbsurdV extends BaseType