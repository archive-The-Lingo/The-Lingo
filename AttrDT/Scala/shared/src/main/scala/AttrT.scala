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

// uses Identifier
sealed trait Exp {
}

// uses VarId
sealed trait Core {
}

sealed trait Attr

sealed trait AttrLevel extends Attr

final case class AttrLevel_UniverseInUniverse() extends AttrLevel

final case class AttrLevel_Known(level: Core) extends AttrLevel

sealed trait AttrSize extends Attr

final case class AttrSize_UnknownFinite() extends AttrSize

final case class AttrSize_Known(size: Core) extends AttrSize

sealed trait AttrUsage extends Attr

final case class AttrUsage_Erased() extends AttrUsage

final case class AttrUsage_Once() extends AttrUsage

final case class AttrUsage_Unlimited() extends AttrUsage

sealed trait AttrSelfUsage extends Attr

final case class AttrSelfUsage_Erased() extends AttrSelfUsage

final case class AttrSelfUsage_Once() extends AttrSelfUsage

final case class AttrSelfUsage_Unlimited() extends AttrSelfUsage

final case class AttrAssumptions(Assumptions: Set[Type]) extends Attr

sealed trait AttrDiverge extends Attr

final case class AttrDiverge_Yes() extends AttrDiverge

final case class AttrDiverge_No() extends AttrDiverge

final case class Attrs(level: AttrLevel, size: AttrSize, usage: AttrUsage, selfUsage: AttrSelfUsage, assumptions: AttrAssumptions, diverge: AttrDiverge)

final case class Type(universe: Exp, attrs: Attrs)

object Exps {
}

object Cores {
}