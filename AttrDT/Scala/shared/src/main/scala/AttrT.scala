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

final case class Context(inner: HashMap[VarId, (Type, Option[Core])]) {
  def updated(id: VarId, t: Type, v: Core): Context = Context(inner.updated(id, (t, Some(v))))

  def updated(id: VarId, t: Type): Context = Context(inner.updated(id, (t, None)))

  def updated(id: Cores.Var, t: Type): Context = this.updated(id.x, t)

  def get(id: VarId): Option[(Type, Option[Core])] = inner.get(id)

  def getType(id: VarId): Option[Type] = inner.get(id).map(_._1)

  def getValue(id: VarId): Option[Core] = inner.get(id).map(_._2).flatten

  def concat(xs: List[(VarId, Type, Core)]): Context = xs match {
    case Nil => this
    case (id, t, v) :: xs => this.updated(id, t, v).concat(xs)
  }
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
  //def subst(context: Context): Core = ???

  def alpha_beta_eta_equals(other: Core, map: AlphaMapping): Boolean = this == other

  final def alpha_beta_eta_equals(other: Core): Boolean = this.alpha_beta_eta_equals(other, AlphaMapping.Empty)

  def weakHeadNormalForm(context: Context): Core = {
    val next = this.reduce(context)
    if (next == this) {
      this
    } else {
      next.weakHeadNormalForm(context)
    }
  }

  def weakHeadNormalForm: Core = this.weakHeadNormalForm(Context.Empty)

  def reduce(context: Context): Core = this

  def infer(context: Context): Option[Type] = {
    val next = this.reduce(context)
    if (next == this) {
      None
    } else {
      next.infer(context)
    }
  }

  def check(context: Context, t: Type): Boolean = this.infer(context) match {
    case Some(t0) => t.alpha_beta_eta_equals(t0)
    case None => {
      val next = this.reduce(context)
      if (next == this) {
        false
      } else {
        next.check(context, t)
      }
    }
  }

  //if (this.check(context, Cores.UniverseInfinite)) {
  //  Some(Type(this.subst(context), ???))
  //} else
  def evalToType(context: Context): Option[Type] = {
    val next = this.reduce(context)
    if (next == this) {
      None
    } else {
      next.evalToType(context)
    }
  }

  def evalToType: Option[Type] = evalToType(Context.Empty)

  final def reducingMatch[A](context: Context, f: Core => Option[A]): Option[A] = f(this) orElse {
    val next = this.reduce(context)
    if (next == this) {
      None
    } else {
      next.reducingMatch(context, f)
    }
  }

  final def reducingMatch(context: Context, f: Core => Boolean): Boolean = f(this) || {
    val next = this.reduce(context)
    if (next == this) {
      false
    } else {
      next.reducingMatch(context, f)
    }
  }
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
  def alpha_beta_eta_equals(other: Attr, map: AlphaMapping): Boolean = this == other

  final def alpha_beta_eta_equals(other: Attr): Boolean = this.alpha_beta_eta_equals(other, AlphaMapping.Empty)
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
  override def alpha_beta_eta_equals(other: Attr, map: AlphaMapping): Boolean = other match {
    case AttrLevel_Known(otherLevel) => level.alpha_beta_eta_equals(otherLevel, map)
    case _ => false
  }
}

sealed trait AttrSize extends Attr {
  def merge(other: AttrSize): AttrSize = (this, other) match {
    case (_, AttrSize_Infinite()) | (AttrSize_Infinite(), _) => AttrSize_Infinite()
    case (_, AttrSize_UnknownFinite()) | (AttrSize_UnknownFinite(), _) => AttrSize_UnknownFinite()
    case (AttrSize_Known(x), AttrSize_Known(y)) => mergeTwoNat(x, y) match {
      case Some(r) => AttrSize_Known(r)
      case None => AttrSize_UnknownFinite()
    }
  }

  def succ: AttrSize = this match {
    case AttrSize_Infinite() | AttrSize_UnknownFinite() => this
    case AttrSize_Known(x) => AttrSize_Known(Cores.Succ(x))
  }
}

object AttrSize {
  val Base = AttrSize_Known(Cores.Zero())
}

final case class AttrSize_UnknownFinite() extends AttrSize

final case class AttrSize_Infinite() extends AttrSize

final case class AttrSize_Known(size: Core) extends AttrSize {
  override def alpha_beta_eta_equals(other: Attr, map: AlphaMapping): Boolean = other match {
    case AttrSize_Known(otherSize) => size.alpha_beta_eta_equals(otherSize, map)
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

  override def alpha_beta_eta_equals(other: Attr, map: AlphaMapping): Boolean = other match {
    case AttrAssumptions(otherAssumptions) if assumptions.size == otherAssumptions.size => {
      val bs = otherAssumptions.toList
      assumptions.toList.permutations.exists((as) => as.zip(bs).forall({ case (x, y) => x.alpha_beta_eta_equals(y, map) }))
    }
    case _ => false
  }
}

object AttrAssumptions {
  def Base = AttrAssumptions(Set())

  private def distinct(xs: List[Type]): List[Type] = xs match {
    case Nil => Nil
    case x :: xs => x :: distinct(xs.filterNot(x.alpha_beta_eta_equals(_)))
  }

  def safeApply(assumptions: Set[Type]): AttrAssumptions = new AttrAssumptions(Set.empty.concat(distinct(assumptions.toList).map(_.erased)))

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

sealed trait AttrRecPi extends Attr {
  def merge(other: AttrRecPi): AttrRecPi = (this, other) match {
    case (_, AttrRecPi_Yes()) | (AttrRecPi_Yes(), _) => AttrRecPi_Yes()
    case (AttrRecPi_No(), AttrRecPi_No()) => AttrRecPi_No()
  }
}

object AttrRecPi {
  val Base = AttrRecPi_No()
}

final case class AttrRecPi_Yes() extends AttrRecPi

final case class AttrRecPi_No() extends AttrRecPi

final case class Attrs(level: AttrLevel, size: AttrSize, usage: AttrUsage, selfUsage: AttrSelfUsage, assumptions: AttrAssumptions, diverge: AttrDiverge, recpi: AttrRecPi) {
  def merge(other: Attrs): Attrs = Attrs(level.merge(other.level), size.merge(other.size), usage.merge(other.usage), selfUsage.merge(other.selfUsage), assumptions.merge(other.assumptions), diverge.merge(other.diverge), recpi.merge(other.recpi))

  // pi is not plain
  // plain: sigma either ...
  def validPlainSubtype(subtype: Attrs): Boolean =
    this.level.merge(subtype.level).alpha_beta_eta_equals(this.level) &&
      this.size.merge(subtype.size.succ).alpha_beta_eta_equals(this.size) &&
      this.usage.merge(subtype.usage).alpha_beta_eta_equals(this.usage) &&
      this.selfUsage.merge(subtype.selfUsage).alpha_beta_eta_equals(this.selfUsage) &&
      this.assumptions.merge(subtype.assumptions).alpha_beta_eta_equals(this.assumptions) &&
      this.diverge.merge(subtype.diverge).alpha_beta_eta_equals(this.diverge) &&
      true // no need to check recpi

  def alpha_beta_eta_equals(other: Attrs, map: AlphaMapping): Boolean =
    level.alpha_beta_eta_equals(other.level, map) &&
      size.alpha_beta_eta_equals(other.size, map) &&
      usage.alpha_beta_eta_equals(other.usage, map) &&
      selfUsage.alpha_beta_eta_equals(other.selfUsage, map) &&
      assumptions.alpha_beta_eta_equals(other.assumptions, map) &&
      diverge.alpha_beta_eta_equals(other.diverge, map) &&
      recpi.alpha_beta_eta_equals(other.recpi, map)

  final def alpha_beta_eta_equals(other: Attrs): Boolean = this.alpha_beta_eta_equals(other, AlphaMapping.Empty)

  def upper: Attrs = Attrs(level.upper, AttrSize.Base, selfUsage.upper, AttrSelfUsage.Base, assumptions, AttrDiverge.Base, AttrRecPi.Base)

  def erased: Attrs = Attrs(level, size, AttrUsage_Erased(), selfUsage, assumptions, diverge, recpi)

  def sized(size: Core): Attrs = Attrs(level, AttrSize_Known(size), usage, selfUsage, assumptions, diverge, recpi)

  def typeInType: Attrs = Attrs(AttrLevel_UniverseInUniverse(), size, usage, selfUsage, assumptions, diverge, recpi)
}

object Attrs {
  val Base = Attrs(AttrLevel.Base, AttrSize.Base, AttrUsage.Base, AttrSelfUsage.Base, AttrAssumptions.Base, AttrDiverge.Base, AttrRecPi.Base)
}

final case class Type(universe: Core, attrs: Attrs) extends Core with CoreInferable {
  override def alpha_beta_eta_equals(other: Core, map: AlphaMapping): Boolean = other match {
    case Type(otherUniverse, otherAttrs) => universe.alpha_beta_eta_equals(otherUniverse, map) && attrs.alpha_beta_eta_equals(otherAttrs, map)
    case _ => false
  }

  def subsetOrEqual(other: Type): Boolean = universe.alpha_beta_eta_equals(other.universe) && (attrs.alpha_beta_eta_equals(other.attrs) || attrs.merge(other.attrs).alpha_beta_eta_equals(attrs))

  def upperType: Type = Type(Cores.Universe(), attrs.upper)

  override def inf(context: Context): Type = upperType

  def erased: Type = Type(universe, attrs.erased)

  def sized(size: Core): Type = Type(universe, attrs.sized(size))

  def typeInType: Type = Type(universe, attrs.typeInType)
}

object Type {
  def apply(universe: Core, attrs: Attrs) = new Type(universe, attrs)

  def apply(universe: Core) = new Type(universe, Attrs.Base)
}

object Exps {
  final case class Var(x: Identifier) extends ExpNeu {
    override def toCore(scope: HashMap[Identifier, VarId]): Cores.Var = scope.get(x) match {
      case Some(v) => Cores.Var(v)
      case None => throw new Error("no definition $x")
    }

    def gen: Cores.Var = Cores.Var(VarId.gen(x))
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

  final case class Kind() extends Exp {
    override def toCore: Core = Cores.Kind()
  }

  final case class MakeKind(x: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = Cores.MakeKind(x.toCore(scope))
  }

  final case class AttrSize(size: Exp, kind: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = Cores.AttrSize(size.toCore(scope), kind.toCore(scope))
  }

  final case class Cons(x: Exp, y: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = Cores.Cons(x.toCore(scope), y.toCore(scope))
  }

  final case class Car(x: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = Cores.Car(x.toCore(scope))
  }

  final case class Cdr(x: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = Cores.Cdr(x.toCore(scope))
  }

  final case class Pi(x: Exp, id: Var, y: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = {
      val id0 = id.gen
      Cores.Pi(x.toCore(scope), id0, y.toCore(scope.updated(id.x, id0.x)))
    }
  }

  abstract class Rec(val id: Var, val kind: Exp, val x: Exp) {
    def toCore(scope: HashMap[Identifier, VarId]): Cores.Rec
  }

  final case class RecCodata(override val id: Var, override val kind: Exp, override val x: Exp) extends Rec(id, kind, x) {
    override def toCore(scope: HashMap[Identifier, VarId]): Cores.Rec = Cores.RecCodata(id.toCore(scope), kind.toCore(scope), x.toCore(scope))
  }

  final case class RecPi(override val id: Var, override val kind: Exp, override val x: Exp) extends Rec(id, kind, x) {
    override def toCore(scope: HashMap[Identifier, VarId]): Cores.Rec = Cores.RecPi(id.toCore(scope), kind.toCore(scope), x.toCore(scope))
  }

  final case class Letrec(bindings: Set[Rec], x: Exp) extends Exp {
    if (bindings.size != bindings.toList.distinctBy(_.id).length) {
      throw new Error("letrec: duplicate id")
    }
    private val recScope: List[(Identifier, VarId)] = bindings.toList.map(_.id).map((id) => (id.x, id.gen.x))

    override def toCore(scope: HashMap[Identifier, VarId]): Core = {
      val ctx: HashMap[Identifier, VarId] = scope.concat(recScope)
      Cores.Letrec(bindings.map(_.toCore(ctx)), x.toCore(ctx))
    }
  }
}

private def transverse[A](xs: List[Option[A]]): Option[List[A]] = xs match {
  case Nil => Some(Nil)
  case Some(x) :: xs => transverse(xs).map(x :: _)
  case None :: _ => None
}

object Cores {
  final case class Var(x: VarId) extends CoreNeu {
    override def infer(context: Context): Option[Type] = context.getType(x)
  }

  private val NatT: Type = Type(Nat())

  final case class Zero() extends Core with CoreInferable {
    override def inf: Type = NatT
  }

  final case class Succ(x: Core) extends Core with CoreInferable {
    override def inf(context: Context): Type = NatT
  }

  private val Universe0: Type = Type(Universe())
  private[AttrT] val UniverseInfinite: Type = Universe0.typeInType
  private val Universe1: Type = Universe0.upperType
  private val Kind0: Type = Type(Kind())
  private val KindInfinite: Type = Kind0.typeInType

  final case class Nat() extends Core with CoreInferable {
    override def inf: Type = Universe0
  }

  // type without attributes
  final case class Universe() extends Core with CoreInferable {
    override def inf: Type = Universe1
  }

  // type with attributes
  final case class Kind() extends Core with CoreInferable {
    override def inf: Type = Universe1
  }

  final case class MakeKind(x: Core) extends Core {
    override def evalToType(context: Context): Option[Type] = if (x.check(context, UniverseInfinite)) {
      Some(Type(x, Attrs.Base))
    } else {
      None
    }

    override def check(context: Context, t: Type): Boolean = t.universe.alpha_beta_eta_equals(Kind()) && x.check(context, Type(Universe(), t.attrs))

    override def infer(context: Context): Option[Type] = if (x.check(context, UniverseInfinite)) {
      x.infer(context) map {
        case Type(_, attrs) => Type(Kind(), attrs.upper)
      }
    } else {
      None
    }
  }

  final case class AttrSize(size: Core, kind: Core) extends Core {
    override def check(context: Context, t: Type): Boolean = size.check(context, NatT) && kind.check(context, KindInfinite) && kind.check(context, t)

    override def infer(context: Context): Option[Type] = if (size.check(context, NatT) && kind.check(context, KindInfinite)) {
      kind.infer(context) // evalToType(context).map(_.upperType)
    } else {
      None
    }

    override def evalToType(context: Context): Option[Type] = if (size.check(context, NatT)) {
      kind.evalToType(context).map(_.sized(size))
    } else {
      None
    }
  }

  final case class Cons(x: Core, y: Core) extends Core {
    override def check(context: Context, t: Type): Boolean = ???
  }

  final case class Car(x: Core) extends CoreNeu {
    override def infer(context: Context): Option[Type] = x.infer(context) flatMap {
      case Type(uni, attrs) => uni.reducingMatch(context, {
        case Pi(a, id, d) => a.evalToType(context)
        case _ => None
      })
    }
  }

  final case class Cdr(x: Core) extends CoreNeu {
    override def infer(context: Context): Option[Type] = x.infer(context) flatMap {
      case Type(uni, attrs) => uni.reducingMatch(context, {
        case Pi(a, id, d) => a.evalToType(context).flatMap((at) => d.evalToType(context.updated(id, at)))
        case _ => None
      })
    }
  }

  final case class Pi(x: Core, id: Var, y: Core) extends Core {
    override def check(context: Context, t: Type): Boolean = ???
  }

  abstract class Rec(val id: Var, val kind: Core, val x: Core)

  final case class RecCodata(override val id: Var, override val kind: Core, override val x: Core) extends Rec(id, kind, x)

  final case class RecPi(override val id: Var, override val kind: Core, override val x: Core) extends Rec(id, kind, x)

  final case class Letrec(bindings: Set[Rec], x: Core) extends Core {
    if (bindings.size != bindings.toList.distinctBy(_.id).length) {
      throw new Error("letrec: duplicate id")
    }

    private def checkBindings(context: Context): Option[Context] = {
      val innerContext0 = context.concat(bindings.toList.map(x => x.kind.evalToType.map(t => (x.id.x, t, x.x))).flatten)

      def step(innerContext: Context) = context.concat(bindings.toList.map(x => x.kind.evalToType(innerContext).map(t => (x.id.x, t, x.x))).flatten)

      val innerContext = step(step(step(step(step(step(step(step(step(step(step(step(step(step(step(step(innerContext0))))))))))))))))
      if (bindings.forall(Letrec.checkBinding(innerContext, _))) {
        Some(innerContext)
      } else {
        None
      }
    }

    override def check(context: Context, t: Type): Boolean = this.checkBindings(context) match {
      case Some(innerContext) => x.check(innerContext, t)
      case None => false
    }
  }

  object Letrec {
    private def checkBinding(context: Context, bind: Rec): Boolean = bind.kind.evalToType(context) match {
      case Some(t) => {
        bind match {
          case RecCodata(id, _, x) => t.attrs.size == AttrSize_Infinite()
          case RecPi(id, _, x) => t.universe.reducingMatch(context, {
            case Pi(arg, argId, result) => arg.evalToType(context) match {
              case Some(t@Type(_, argAttrs)) => {
                val resultContext = context.updated(argId, t)
                argAttrs.size match {
                  case AttrSize_Infinite() | AttrSize_UnknownFinite() => result.evalToType(resultContext) match {
                    case Some(Type(_, resultAttrs)) => resultAttrs.diverge == AttrDiverge_Yes()
                    case None => false
                  }
                  case AttrSize_Known(size) => ???
                }
              }
              case None => false
            }
            case _ => false
          })
        }
      }
      case None => false
    }
  }
}