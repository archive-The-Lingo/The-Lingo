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

final case class AttributeAssumptions(assumption: Set[Type])

final case class Attrbutes(level: AttributeLevel, size: Option[AttributeSize], usage: AttributeUsage, dynamic: Boolean, assumptions: AttributeAssumptions)

final case class Type(t: BaseType, attr: Attrbutes) extends Value

sealed trait Neu extends Value

final case class NeuVar(x: VarId) extends Neu

case object Zero extends Value

final case class Succ(x: Value) extends Value

def natToValue(x: Nat): Value = if (x == 0) Zero else Succ(natToValue(x - 1))