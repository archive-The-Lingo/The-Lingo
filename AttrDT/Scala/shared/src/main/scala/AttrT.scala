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

object Context {
  val Empty = Context(HashMap())
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
  def weakHeadNormalForm: Exp = ???

  def toCore(scope: HashMap[Identifier, VarId]): Core = this.toCore

  def toCore: Core = this.toCore(HashMap())
}

// is neutral if appers in normal form
sealed trait ExpNeu extends Exp

sealed trait AlphaEtaEqual {

}

// uses VarId
sealed trait Core {
  def alpha_eta_equals(other: Core, map: AlphaMapping): Boolean = this == other

  final def alpha_eta_equals(other: Core): Boolean = this.alpha_eta_equals(other, AlphaMapping.Empty)

  def weakHeadNormalForm: Core = ???

  def infer(context: Context): Option[Type] = None

  def check(context: Context, t: Type): Boolean
}

sealed trait CoreInferable extends Core {
  final override def infer(context: Context): Option[Type] = Some(inf(context))

  def inf(context: Context): Type = this.inf

  def inf: Type = this.inf(Context.Empty)

  final override def check(context: Context, t: Type): Boolean = t.subsetOrEqual(this.inf(context))
}

// is neutral if appers in normal form
sealed trait CoreNeu extends Core

sealed trait Attr {
  def alpha_eta_equals(other: Attr, map: AlphaMapping): Boolean = this == other

  final def alpha_eta_equals(other: Attr): Boolean = this.alpha_eta_equals(other, AlphaMapping.Empty)
}

private sealed trait NatParseResult

private case class NatParseResult_Just(x: NaturalNumber) extends NatParseResult

private case class NatParseResult_SuccNeu(x: NaturalNumber, neu: CoreNeu) extends NatParseResult

private def parseNat(x: Core): Option[NatParseResult] = x.weakHeadNormalForm match {
  case Cores.Zero() => Some(NatParseResult_Just(0))
  case Cores.Succ(x) => parseNat(x) map {
    case NatParseResult_Just(v) => NatParseResult_Just(v + 1)
    case NatParseResult_SuccNeu(v, neu) => NatParseResult_SuccNeu(v + 1, neu)
  }
  case neu: CoreNeu => Some(NatParseResult_SuccNeu(0, neu))
  case _ => None
}

private def natToCore(x: NaturalNumber, base: Core = Cores.Zero()): Core = if (x == 0) base else Cores.Succ(natToCore(x - 1))

def mergeTwoNat(x: Core, y: Core): Option[Core] = if (x == y) Some(x) else (for {x <- parseNat(x); y <- parseNat(y)} yield (x, y)) flatMap {
  case (NatParseResult_Just(x), NatParseResult_Just(y)) => Some(natToCore(x.max(y)))
  case (NatParseResult_SuccNeu(x, xNeu), NatParseResult_SuccNeu(y, yNeu)) => if (xNeu == yNeu) Some(natToCore(x.max(y), xNeu)) else None
  // following are required and work pretty well if y is zero
  case (NatParseResult_SuccNeu(x, xNeu), NatParseResult_Just(y)) => Some(natToCore(x.max(y), xNeu))
  case (NatParseResult_Just(y), NatParseResult_SuccNeu(x, xNeu)) => Some(natToCore(x.max(y), xNeu))
}

sealed trait AttrLevel extends Attr {
  def merge(other: AttrLevel): AttrLevel = (this, other) match {
    case (_, AttrLevel_UniverseInUniverse()) | (AttrLevel_UniverseInUniverse(), _) => AttrLevel_UniverseInUniverse()
    case (AttrLevel_Known(x), AttrLevel_Known(y)) => mergeTwoNat(x, y) match {
      case Some(r) => AttrLevel_Known(r)
      case None => AttrLevel_UniverseInUniverse()
    }
  }

  def upper: AttrLevel = this match {
    case AttrLevel_UniverseInUniverse() => AttrLevel_UniverseInUniverse()
    case AttrLevel_Known(level) => AttrLevel_Known(Cores.Succ(level))
  }
}

object AttrLevel {
  val Base = AttrLevel_Known(Cores.Zero())
}

final case class AttrLevel_UniverseInUniverse() extends AttrLevel

final case class AttrLevel_Known(level: Core) extends AttrLevel {
  override def alpha_eta_equals(other: Attr, map: AlphaMapping): Boolean = other match {
    case AttrLevel_Known(otherLevel) => level.alpha_eta_equals(otherLevel, map)
    case _ => false
  }
}

sealed trait AttrSize extends Attr {
  def merge(other: AttrSize): AttrSize = (this, other) match {
    case (_, AttrSize_UnknownFinite()) | (AttrSize_UnknownFinite(), _) => AttrSize_UnknownFinite()
    case (AttrSize_Known(x), AttrSize_Known(y)) => mergeTwoNat(x, y) match {
      case Some(r) => AttrSize_Known(r)
      case None => AttrSize_UnknownFinite()
    }
  }
}

object AttrSize {
  val Base = AttrSize_Known(Cores.Zero())
}

final case class AttrSize_UnknownFinite() extends AttrSize

final case class AttrSize_Known(size: Core) extends AttrSize {
  override def alpha_eta_equals(other: Attr, map: AlphaMapping): Boolean = other match {
    case AttrSize_Known(otherSize) => size.alpha_eta_equals(otherSize, map)
    case _ => false
  }
}

sealed trait AttrUsage extends Attr {
  def merge(other: AttrUsage): AttrUsage = (this, other) match {
    case (AttrUsage_Erased(), _) | (_, AttrUsage_Erased()) => AttrUsage_Erased()
    case (AttrUsage_Once(), _) | (_, AttrUsage_Once()) => AttrUsage_Once()
    case (AttrUsage_Unlimited(), AttrUsage_Unlimited()) => AttrUsage_Unlimited()
  }
}

object AttrUsage {
  val Base = AttrUsage_Unlimited()
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

  def upper: AttrUsage = this match {
    case AttrSelfUsage_Erased() => AttrUsage_Erased()
    case AttrSelfUsage_Once() => AttrUsage_Once()
    case AttrSelfUsage_Unlimited() => AttrUsage_Unlimited()
  }
}

object AttrSelfUsage {
  val Base = AttrSelfUsage_Unlimited()
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
  def Base = AttrAssumptions(Set())

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

object AttrDiverge {
  val Base = AttrDiverge_No()
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

  final def alpha_eta_equals(other: Attrs): Boolean = this.alpha_eta_equals(other, AlphaMapping.Empty)

  def upper: Attrs = Attrs(level.upper, AttrSize.Base, selfUsage.upper, AttrSelfUsage.Base, assumptions, AttrDiverge.Base)
}

object Attrs {
  val Base = Attrs(AttrLevel.Base, AttrSize.Base, AttrUsage.Base, AttrSelfUsage.Base, AttrAssumptions.Base, AttrDiverge.Base)
}

final case class Type(universe: Core, attrs: Attrs) extends Core with CoreInferable {
  override def alpha_eta_equals(other: Core, map: AlphaMapping): Boolean = other match {
    case Type(otherUniverse, otherAttrs) => universe.alpha_eta_equals(otherUniverse, map) && attrs.alpha_eta_equals(otherAttrs, map)
    case _ => false
  }

  def subsetOrEqual(other: Type): Boolean = universe.alpha_eta_equals(other.universe) && (attrs.alpha_eta_equals(other.attrs) || attrs.merge(other.attrs).alpha_eta_equals(attrs))

  override def inf(context: Context): Type = Type(Cores.Universe(), attrs.upper)
}

object Exps {
  final case class Var(x: Identifier) extends ExpNeu {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = scope.get(x) match {
      case Some(v) => Cores.Var(v)
      case None => throw new Error("no definition $x")
    }
  }

  final case class Zero() extends Exp {
    override def toCore: Core = Cores.Zero()
  }

  final case class Succ(x: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = Cores.Succ(x.toCore(scope))
  }

  final case class Nat() extends Exp {
    override def toCore: Core = Cores.Nat()
  }

  final case class Universe() extends Exp {
    override def toCore: Core = Cores.Universe()
  }
}

object Cores {
  final case class Var(x: VarId) extends CoreNeu with CoreInferable {
    override def inf(context: Context): Type = context.getType(x) match {
      case Some(t) => t
      case None => throw new Error("no definition $x")
    }
  }

  final case class Zero() extends Core with CoreInferable {
    override def inf: Type = Type(Nat(), Attrs.Base)
  }

  final case class Nat() extends Core with CoreInferable {
    override def inf: Type = ???
  }

  final case class Succ(x: Core) extends Core with CoreInferable {
    override def inf(context: Context): Type = ???
  }

  final case class Universe() extends Core with CoreInferable {
    override def inf: Type = Type(Universe(), Attrs.Base.upper)
  }
}