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

final case class AlphaMapping(inner: HashMap[VarId, VarId], reverseMap: HashMap[VarId, VarId]) {
  def has(a: VarId, b: VarId): Boolean = inner.get(a) match {
    case Some(b0) => b == b0
    case None => false
  }

  def add(a: VarId, b: VarId): AlphaMapping = inner.get(a) match {
    case Some(b0) => if (b == b0) this else throw Error("duplicate")
    case None => reverseMap.get(b) match {
      case Some(a0) => if (a == a0) throw Error("Illegal State") else throw Error("duplicate")
      case None => AlphaMapping(inner.updated(a, b), reverseMap.updated(b, a))
    }
  }

  def reverse: AlphaMapping = AlphaMapping(reverseMap, inner)
}

sealed trait SpecialUsage

case object SpecialUsageErased extends SpecialUsage

case object SpecialUsageOnce extends SpecialUsage

final case class UsageMap(inner: HashMap[VarId, SpecialUsage]) {
  def use(x: VarId): Option[UsageMap] = inner.get(x) match {
    case None => Some(this)
    case Some(SpecialUsageErased) => None
    case Some(SpecialUsageOnce) => Some(UsageMap(inner.updated(x, SpecialUsageErased)))
  }

  def markErased(x: VarId): UsageMap = inner.get(x) match {
    case None => UsageMap(inner.updated(x, SpecialUsageErased))
    case Some(_) => throw Error("duplicate")
  }

  def markOnce(x: VarId): UsageMap = inner.get(x) match {
    case None => UsageMap(inner.updated(x, SpecialUsageOnce))
    case Some(_) => throw Error("duplicate")
  }
}

type UsageUpdate = Set[VarId] // used <once>

sealed trait Value {
  def readback(t: Type): Exp = ???

  def alpha_eta_equivalent(other: Value, map: AlphaMapping): Boolean

  def check_usage(t: Type, usageMap: UsageMap): Option[UsageMap] = ???
}

sealed trait Exp {
  def eval(env: Env): Value = ???

  // usage is not checked here
  def check(env: Env, t: Type): Boolean = ???

  def infer(env: Env): Option[Type] = None
}

sealed trait BaseTypeOrNotYet

sealed trait BaseType extends BaseTypeOrNotYet {
  def alpha_eta_equivalent(other: BaseType, map: AlphaMapping): Boolean = ???
}

sealed trait Attribute {
  def alpha_eta_equivalent(other: Attribute, map: AlphaMapping): Boolean
}

sealed trait AttributeLevel extends Attribute

final case class AttributeLevelKnown(x: Value) extends AttributeLevel {
  override def alpha_eta_equivalent(other: Attribute, map: AlphaMapping): Boolean = other match {
    case AttributeLevelKnown(y) => x.alpha_eta_equivalent(y, map)
    case _ => false
  }
}

case object AttributeLevelTypeInType extends AttributeLevel {
  override def alpha_eta_equivalent(other: Attribute, _map: AlphaMapping): Boolean = this == other
}

sealed trait AttributeSize extends Attribute

final case class AttributeSizeKnown(x: Value) extends AttributeSize {
  override def alpha_eta_equivalent(other: Attribute, map: AlphaMapping): Boolean = other match {
    case AttributeSizeKnown(y) => x.alpha_eta_equivalent(y, map)
    case _ => false
  }
}

case object AttributeSizeFinite extends AttributeSize {
  override def alpha_eta_equivalent(other: Attribute, _map: AlphaMapping): Boolean = this == other
}

sealed trait AttributeUsage extends Attribute {
  override def alpha_eta_equivalent(other: Attribute, _map: AlphaMapping): Boolean = this == other
}

case object AttributeUsageErased extends AttributeUsage

case object AttributeUsageOnce extends AttributeUsage

case object AttributeUsageNotLimited extends AttributeUsage

sealed trait AttributeSelfUsage extends Attribute {
  override def alpha_eta_equivalent(other: Attribute, _map: AlphaMapping): Boolean = this == other
}

case object AttributeSelfUsageErased extends AttributeSelfUsage

case object AttributeSelfUsageOnce extends AttributeSelfUsage

case object AttributeSelfUsageNotLimited extends AttributeSelfUsage

final case class AttributeAssumptions(assumptions: Set[Type]) extends Attribute {
  override def alpha_eta_equivalent(other: Attribute, map: AlphaMapping): Boolean = other match {
    case AttributeAssumptions(otherAssumptions) if assumptions.size == otherAssumptions.size => {
      val bs = otherAssumptions.toList
      assumptions.toList.permutations.exists((as) => as.zip(bs).forall({ case (x, y) => x.alpha_eta_equivalent(y, map) }))
    }
    case _ => false
  }
}

sealed trait AttributeDiverge extends Attribute {
  override def alpha_eta_equivalent(other: Attribute, _map: AlphaMapping): Boolean = this == other
}

case object AttributeDivergeYes extends AttributeDiverge

case object AttributeDivergeNo extends AttributeDiverge

final case class Attrbutes(level: AttributeLevel, size: AttributeSize, usage: AttributeUsage, selfUsage: AttributeSelfUsage, diverge: AttributeDiverge, assumptions: AttributeAssumptions) {
  def alpha_eta_equivalent(other: Attrbutes, map: AlphaMapping): Boolean =
    level.alpha_eta_equivalent(other.level, map) &&
      size.alpha_eta_equivalent(other.size, map) &&
      usage.alpha_eta_equivalent(other.usage, map) &&
      selfUsage.alpha_eta_equivalent(other.selfUsage, map) &&
      diverge.alpha_eta_equivalent(other.diverge, map) &&
      assumptions.alpha_eta_equivalent(other.assumptions, map)
}

sealed trait TypeOrNotYet extends Value

final case class Type(t: BaseType, attr: Attrbutes) extends Value with TypeOrNotYet {
  override def alpha_eta_equivalent(other: Value, map: AlphaMapping): Boolean = other match {
    case Type(t2, attr2) => t.alpha_eta_equivalent(t2, map) && attr.alpha_eta_equivalent(attr2, map)
    case _ => false
  }

  def mergeAttrbutes(moreAttr: Attrbutes): Type = ???
}

case object Kind extends Exp

case object KindV extends BaseType

final case class NotYetValue(t: Type, neu: Neu) extends Value with TypeOrNotYet with BaseTypeOrNotYet {
  override def alpha_eta_equivalent(other: Value, map: AlphaMapping): Boolean = other match {
    case NotYetValue(t2, neu2) => t.alpha_eta_equivalent(t2, map) && neu.alpha_eta_equivalent(neu2, map)
    case _ => other.alpha_eta_equivalent(this, map.reverse) // eta
  }
}

sealed trait Neu {
  def alpha_eta_equivalent(other: Neu, map: AlphaMapping): Boolean = ???
}

final case class NeuVar(x: VarId) extends Neu {
  override def alpha_eta_equivalent(other: Neu, map: AlphaMapping): Boolean = other match {
    case NeuVar(y) => map.has(x, y)
    case _ => false
  }
}

case object ZeroV extends Value {
  override def alpha_eta_equivalent(other: Value, _map: AlphaMapping): Boolean = this == other
}

case object Zero extends Exp

final case class SuccV(x: Value) extends Value {
  override def alpha_eta_equivalent(other: Value, map: AlphaMapping): Boolean = other match {
    case SuccV(y) => x.alpha_eta_equivalent(y, map)
    case _ => false
  }
}

final case class Succ(x: Exp) extends Exp

case object NatV extends BaseType {
  override def alpha_eta_equivalent(other: BaseType, _map: AlphaMapping): Boolean = this == other
}

case object Nat extends Exp

def natToValue(x: NaturalNumber): Value = if (x == 0) ZeroV else SuccV(natToValue(x - 1))

final case class Sym(x: String) extends Value {
  override def alpha_eta_equivalent(other: Value, _map: AlphaMapping): Boolean = this == other
}

final case class Quote(x: String) extends Exp

case object Atom extends Exp

case object AtomV extends BaseType {
  override def alpha_eta_equivalent(other: BaseType, _map: AlphaMapping): Boolean = this == other
}

case object UnitV extends Value {
  override def alpha_eta_equivalent(other: Value, _map: AlphaMapping): Boolean = this == other
}

case object Unit extends Exp

case object TrivialV extends BaseType {
  override def alpha_eta_equivalent(other: BaseType, _map: AlphaMapping): Boolean = this == other
}

case object Trivial extends Exp

final case class ConsV(a: Value, d: Value) extends Value {
  override def alpha_eta_equivalent(other: Value, map: AlphaMapping): Boolean = other match {
    case ConsV(a1, d1) => a.alpha_eta_equivalent(a1, map) && d.alpha_eta_equivalent(d1, map)
    case NotYetValue(Type(SigmaV(sa, sd), attr), neu) => {
      val newa = NotYetValue(sa.mergeAttrbutes(attr), NeuCar(neu))
      sd.apply(sa) match {
        case td: Type => {
          val newb = NotYetValue(td.mergeAttrbutes(attr), NeuCdr(neu))
          this.alpha_eta_equivalent(ConsV(newa, newb), map)
        }
        case _ => throw new Error("not Kind")
      }
    }
    case _ => false
  }
}

final case class Cons(a: Exp, d: Exp) extends Exp

final case class SigmaV(a: Type, d: Closure) extends BaseType

final case class Sigma(a: Exp, aId: Identifier, d: Exp) extends Exp

final case class Closure(id: NeuVar, body: Value) extends Value {
  def apply(x: Value): Value = ???

  override def alpha_eta_equivalent(other: Value, map: AlphaMapping): Boolean = ???
}

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

case object SameV extends Value {
  override def alpha_eta_equivalent(other: Value, _map: AlphaMapping): Boolean = this == other
}

final case class Attributed(attr: List[Attribute], t: Exp) extends Exp

case object Absurd extends Exp

case object AbsurdV extends BaseType