package AttrT

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

final case class Context(inner: HashMap[VarId, (Type, Exp)]) {
  def updated(id: VarId, t: Type, v: Exp): Context = Context(inner.updated(id, (t, v)))

  def get(id: VarId): Option[(Type, Exp)] = inner.get(id)

  def getType(id: VarId): Option[Type] = inner.get(id).map(_._1)

  def getValue(id: VarId): Option[Exp] = inner.get(id).map(_._2)
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

object AlphaMapping {
  val Empty: AlphaMapping = AlphaMapping(HashMap(), HashMap())
}

// uses Identifier
sealed trait Exp {
}

sealed trait AlphaEtaEqual {

}

// uses VarId
sealed trait Core {
  def alpha_eta_equals(other: Core, map: AlphaMapping): Boolean = this == other

  final def alpha_eta_equals(other: Core): Boolean = this.alpha_eta_equals(other, AlphaMapping.Empty)
}

sealed trait Attr {
  def alpha_eta_equals(other: Attr, map: AlphaMapping): Boolean = this == other

  final def alpha_eta_equals(other: Attr): Boolean = this.alpha_eta_equals(other, AlphaMapping.Empty)
}

sealed trait AttrLevel extends Attr {
  def merge(other: AttrLevel): AttrLevel = (this, other) match {
    case (_, AttrLevel_UniverseInUniverse()) | (AttrLevel_UniverseInUniverse(), _) => AttrLevel_UniverseInUniverse()
    case (x: AttrLevel_Known, y: AttrLevel_Known) => x.merge0(y)
  }
}

final case class AttrLevel_UniverseInUniverse() extends AttrLevel

final case class AttrLevel_Known(level: Core) extends AttrLevel {
  def merge0(other: AttrLevel_Known): AttrLevel_Known = ???
}

sealed trait AttrSize extends Attr {
  def merge(other: AttrSize): AttrSize = (this, other) match {
    case (_, AttrSize_UnknownFinite()) | (AttrSize_UnknownFinite(), _) => AttrSize_UnknownFinite()
    case (x: AttrSize_Known, y: AttrSize_Known) => x.merge0(y)
  }
}

final case class AttrSize_UnknownFinite() extends AttrSize

final case class AttrSize_Known(size: Core) extends AttrSize {
  override def alpha_eta_equals(other: Attr, map: AlphaMapping): Boolean = other match {
    case AttrSize_Known(otherSize) => size.alpha_eta_equals(otherSize, map)
    case _ => false
  }

  def merge0(other: AttrSize_Known): AttrSize_Known = ???
}

sealed trait AttrUsage extends Attr {
  def merge(other: AttrUsage): AttrUsage = (this, other) match {
    case (AttrUsage_Erased(), _) | (_, AttrUsage_Erased()) => AttrUsage_Erased()
    case (AttrUsage_Once(), _) | (_, AttrUsage_Once()) => AttrUsage_Once()
    case (AttrUsage_Unlimited(), AttrUsage_Unlimited()) => AttrUsage_Unlimited()
  }
}

final case class AttrUsage_Erased() extends AttrUsage

final case class AttrUsage_Once() extends AttrUsage

final case class AttrUsage_Unlimited() extends AttrUsage

sealed trait AttrSelfUsage extends Attr {
  def merge(other: AttrSelfUsage): AttrSelfUsage = (this, other) match {
    case (AttrSelfUsage_Erased(), _) | (_, AttrSelfUsage_Erased()) => AttrSelfUsage_Erased()
    case (AttrSelfUsage_Once(), _) | (_, AttrSelfUsage_Once()) => AttrSelfUsage_Once()
    case (AttrSelfUsage_Unlimited(), AttrSelfUsage_Unlimited()) => AttrSelfUsage_Unlimited()
  }
}

final case class AttrSelfUsage_Erased() extends AttrSelfUsage

final case class AttrSelfUsage_Once() extends AttrSelfUsage

final case class AttrSelfUsage_Unlimited() extends AttrSelfUsage

final case class AttrAssumptions(assumptions: Set[Type]) extends Attr {
  def merge(other: AttrAssumptions): AttrAssumptions = AttrAssumptions.safeApply(assumptions.union(other.assumptions))

  override def alpha_eta_equals(other: Attr, map: AlphaMapping): Boolean = other match {
    case AttrAssumptions(otherAssumptions) if assumptions.size == otherAssumptions.size => {
      val bs = otherAssumptions.toList
      assumptions.toList.permutations.exists((as) => as.zip(bs).forall({ case (x, y) => x.alpha_eta_equals(y, map) }))
    }
    case _ => false
  }
}

object AttrAssumptions {
  private def distinct(xs: List[Type]): List[Type] = xs match {
    case Nil => Nil
    case x :: xs => x :: distinct(xs.filterNot(x.alpha_eta_equals(_)))
  }

  def safeApply(assumptions: Set[Type]): AttrAssumptions = new AttrAssumptions(Set.empty.concat(distinct(assumptions.toList)))

  def apply(assumptions: Set[Type]): AttrAssumptions = safeApply(assumptions)
}

sealed trait AttrDiverge extends Attr {
  def merge(other: AttrDiverge): AttrDiverge = (this, other) match {
    case (AttrDiverge_Yes(), _) | (_, AttrDiverge_Yes()) => AttrDiverge_Yes()
    case (AttrDiverge_No(), AttrDiverge_No()) => AttrDiverge_No()
  }
}

final case class AttrDiverge_Yes() extends AttrDiverge

final case class AttrDiverge_No() extends AttrDiverge

final case class Attrs(level: AttrLevel, size: AttrSize, usage: AttrUsage, selfUsage: AttrSelfUsage, assumptions: AttrAssumptions, diverge: AttrDiverge) {
  def merge(other: Attrs): Attrs = Attrs(level.merge(other.level), size.merge(other.size), usage.merge(other.usage), selfUsage.merge(other.selfUsage), assumptions.merge(other.assumptions), diverge.merge(other.diverge))

  def alpha_eta_equals(other: Attrs, map: AlphaMapping): Boolean =
    level.alpha_eta_equals(other.level, map) &&
      size.alpha_eta_equals(other.size, map) &&
      usage.alpha_eta_equals(other.usage, map) &&
      selfUsage.alpha_eta_equals(other.selfUsage, map) &&
      assumptions.alpha_eta_equals(other.assumptions, map) &&
      diverge.alpha_eta_equals(other.diverge, map)
}

final case class Type(universe: Core, attrs: Attrs) extends Core {
  override def alpha_eta_equals(other: Core, map: AlphaMapping): Boolean = other match {
    case Type(otherUniverse, otherAttrs) => universe.alpha_eta_equals(otherUniverse, map) && attrs.alpha_eta_equals(otherAttrs, map)
    case _ => false
  }
}

object Exps {
}

object Cores {
}