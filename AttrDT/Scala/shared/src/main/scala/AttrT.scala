package AttrT

import scala.collection.immutable.HashMap
import scala.language.implicitConversions

type Identifier = Symbol

type UniqueIdentifier = Int

type NaturalNumber = Int

final case class Error(x: String) extends Exception(x)

sealed abstract class Err(x: String) extends Exception(x)

final case class ErrCantInfer(context: Context, x: Core) extends Err(s"can't infer $x in the context $context")

final case class ErrCheckFailed(context: Context, expectedType: Core, x: Core, realType: Core) extends Err(s"check $x failed in the context $context expect $expectedType, got $realType")

final case class ErrExpected(context: Context, expectedType: String, x: Core, realType: Core) extends Err(s"expect $x to be $expectedType in the context $context, got $realType")

final case class ErrExpectedV(context: Context, expectedType: String, x: Core) extends Err(s"expect $x to be $expectedType in the context $context")

final case class ErrTypeUnknown(context: Context, x: Cores.Var) extends Err(s"the type of $x is unknown in the context $context")

final case class ErrCantEvalToType(context: Context, x: Core) extends Err(s"$x in the context $context can't be a type")

final case class ErrLetrec(context: Context, x: Core) extends Err(s"illegal letrec $x in the context $context")

final case class ErrDiverge(context: Context, x: Cores.Rec) extends Err(s"expected diverge for $x in the context $context")

final case class ErrUnknownFiniteRec(context: Context, id: Core, x: Core, t: Core) extends Err(s"don't know how to check UnknownFinite $id $x: $t in $context")

final case class ErrNotDivergePiRec(context: Context, id: Core, x: Core, t: Core) extends Err(s"don't know how to check non-diverge Pi $id $x: $t in $context")

final case class ErrRecs(context: Context, errs: List[Err]) extends Err(s"Recs failed in $context caused by $errs")

final case class ErrUnknownTypeRec(context: Context, id: Core, x: Core, t: Core) extends Err(s"don't know how to check $id $x: $t with unknown type in $context")

final case class ErrExpectedCodata(context: Context, x: Core, t: Core) extends Err(s"expected $x to be codata in the context $context, got $t")


final case class ErrPlainSubtype(t: Core, sub: Core) extends Err(s"$sub can't be a plain subtype of $t")

final case class ErrWeakSubtype(t: Core, sub: Core) extends Err(s"$sub can't be a weak subtype of $t")

type Maybe[T] = Either[Err, T]
private implicit def someToRight[T, U](x: Some[T]): Right[U, T] = x match {
  case Some(x) => Right(x)
}
private implicit def eitherToBoolean[T, U](x: Either[T, U]): Boolean = x match {
  case Right(_) => true
  case Left(_) => false
}
private implicit def eitherErase[T, U](x: Either[T, U]): Either[T, Unit] = x match {
  case Right(_) => Right(())
  case Left(v) => Left(v)
}

private implicit final class EitherAnd[T, U](self: Either[T, U]) {
  def and[U1](other: => Either[T, U1]): Either[T, (U, U1)] = self match {
    case Left(x) => Left(x)
    case Right(a) => other match {
      case Left(x) => Left(x)
      case Right(b) => Right((a, b))
    }
  }
}

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

final case class Context(context: HashMap[VarId, (Type, Option[Core])]) {
  def updated(id: VarId, t: Type, v: Option[Core]): Context = Context(context.updated(id, (t, v)))

  def updated(id: Cores.Var, t: Type, v: Option[Core]): Context = this.updated(id.x, t, v)

  def updated(id: VarId, t: Type, v: Core): Context = Context(context.updated(id, (t, Some(v))))

  def updated(id: Cores.Var, t: Type, v: Core): Context = this.updated(id.x, t, v)

  def updated(id: VarId, t: Type): Context = Context(context.updated(id, (t, None)))

  def updated(id: Cores.Var, t: Type): Context = this.updated(id.x, t)

  def get(id: VarId): Option[(Type, Option[Core])] = context.get(id)

  def get(id: Cores.Var): Option[(Type, Option[Core])] = this.get(id.x)

  def getType(id: VarId): Option[Type] = context.get(id).map(_._1)

  def getType(v: Cores.Var): Option[Type] = this.getType(v.x)

  def getValue(id: VarId): Option[Core] = context.get(id).map(_._2).flatten

  def getValue(id: Cores.Var): Option[Core] = this.getValue(id.x)

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
type Subst = HashMap[Cores.Var, Core]

sealed trait Core {
  def subst(s: Subst): Core

  final def scanVar(v: Cores.Var): NaturalNumber = ???

  def scan: List[Core]

  final def subst(v: Cores.Var, x: Core): Core = this.subst(HashMap((v, x)))

  final def subst(v: Cores.Var, t: Type, x: Core): Core = this.subst(HashMap((v, Cores.InternalThe(t, x))))

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
  // def reduce(context: Context): Maybe[Context, Core] = ???

  def infer(context: Context): Maybe[Type] = {
    val next = this.reduce(context)
    if (next == this) {
      Left(ErrCantInfer(context, this))
    } else {
      next.infer(context)
    }
  }

  def check(context: Context, t: Type): Maybe[Unit] = this.infer(context) match {
    case Right(t0) => if (t.subsetOrEqual(t0)) {
      Right(())
    } else {
      Left(ErrCheckFailed(context, t, this, t0))
    }
    case Left(err) => {
      val next = this.reduce(context)
      if (next == this) {
        Left(err)
      } else {
        next.check(context, t)
      }
    }
  }

  //if (this.check(context, Cores.UniverseInfinite)) {
  //  Some(Type(this.subst(context), ???))
  //} else
  def evalToType(context: Context): Maybe[Type] = {
    val next = this.reduce(context)
    if (next == this) {
      Left(ErrCantEvalToType(context, this))
    } else {
      next.evalToType(context)
    }
  }

  def evalToType: Maybe[Type] = evalToType(Context.Empty)

  final def reducingMatch[A](context: Context, f: Core => Option[A]): Option[A] = f(this) orElse {
    val next = this.reduce(context)
    if (next == this) {
      None
    } else {
      next.reducingMatch(context, f)
    }
  }

  final def reducingMatch[A](context: Context, f: Core => Maybe[A]): Maybe[A] = f(this) match {
    case Left(err) => {
      val next = this.reduce(context)
      if (next == this) {
        Left(err)
      } else {
        next.reducingMatch(context, f)
      }
    }
    case Right(v) => Right(v)
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

sealed trait CoreType extends Core {
  final override def infer(context: Context): Maybe[Type] = this.evalToType(context).map(_.upperType)
}

// is neutral if appers in normal form
sealed trait CoreNeu extends Core

sealed trait Attr {
  def scan: List[Core] = List()

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
  def subst(s: Subst): AttrLevel = this match {
    case AttrLevel_Known(x) => AttrLevel_Known(x.subst(s))
    case other@(AttrLevel_UniverseInUniverse()) => other
  }

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
  override def scan: List[Core] = List(level)

  override def alpha_beta_eta_equals(other: Attr, map: AlphaMapping): Boolean = other match {
    case AttrLevel_Known(otherLevel) => level.alpha_beta_eta_equals(otherLevel, map)
    case _ => false
  }
}

sealed trait AttrSize extends Attr {
  def subst(s: Subst): AttrSize = this match {
    case AttrSize_Known(x) => AttrSize_Known(x.subst(s))
    case other@(AttrSize_Infinite() | AttrSize_UnknownFinite()) => other
  }

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
  override def scan: List[Core] = List(size)

  override def alpha_beta_eta_equals(other: Attr, map: AlphaMapping): Boolean = other match {
    case AttrSize_Known(otherSize) => size.alpha_beta_eta_equals(otherSize, map)
    case _ => false
  }
}

sealed trait AttrUsage extends Attr {
  def subst(s: Subst): AttrUsage = this

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
  def subst(s: Subst): AttrSelfUsage = this

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
  override def scan: List[Core] = assumptions.toList

  def subst(s: Subst): AttrAssumptions = AttrAssumptions.safeApply(assumptions.map(_.subst(s)))

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
  def subst(s: Subst): AttrDiverge = this

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
  def scan: List[Core] = level.scan ++ size.scan ++ usage.scan ++ selfUsage.scan ++ assumptions.scan ++ diverge.scan

  def subst(s: Subst): Attrs = Attrs(level.subst(s), size.subst(s), usage.subst(s), selfUsage.subst(s), assumptions.subst(s), diverge.subst(s))

  def merge(other: Attrs): Attrs = Attrs(level.merge(other.level), size.merge(other.size), usage.merge(other.usage), selfUsage.merge(other.selfUsage), assumptions.merge(other.assumptions), diverge.merge(other.diverge))

  // pi is not plain
  // plain: sigma either ...
  def validPlainSubtype(subtype: Attrs): Boolean =
    this.level.merge(subtype.level).alpha_beta_eta_equals(this.level) &&
      this.size.merge(subtype.size.succ).alpha_beta_eta_equals(this.size) &&
      this.usage.merge(subtype.usage).alpha_beta_eta_equals(this.usage) &&
      this.selfUsage.merge(subtype.selfUsage).alpha_beta_eta_equals(this.selfUsage) &&
      this.assumptions.merge(subtype.assumptions).alpha_beta_eta_equals(this.assumptions) &&
      this.diverge.merge(subtype.diverge).alpha_beta_eta_equals(this.diverge)

  def validWeakSubtype(subtype: Attrs): Boolean = this.level.merge(subtype.level).alpha_beta_eta_equals(this.level)

  def alpha_beta_eta_equals(other: Attrs, map: AlphaMapping): Boolean =
    level.alpha_beta_eta_equals(other.level, map) &&
      size.alpha_beta_eta_equals(other.size, map) &&
      usage.alpha_beta_eta_equals(other.usage, map) &&
      selfUsage.alpha_beta_eta_equals(other.selfUsage, map) &&
      assumptions.alpha_beta_eta_equals(other.assumptions, map) &&
      diverge.alpha_beta_eta_equals(other.diverge, map)

  final def alpha_beta_eta_equals(other: Attrs): Boolean = this.alpha_beta_eta_equals(other, AlphaMapping.Empty)

  def upper: Attrs = Attrs(level.upper, AttrSize.Base, selfUsage.upper, AttrSelfUsage.Base, assumptions, AttrDiverge.Base)

  def erased: Attrs = Attrs(level, size, AttrUsage_Erased(), selfUsage, assumptions, diverge)

  def sized(size: Core): Attrs = Attrs(level, AttrSize_Known(size), usage, selfUsage, assumptions, diverge)

  def typeInType: Attrs = Attrs(AttrLevel_UniverseInUniverse(), size, usage, selfUsage, assumptions, diverge)
}

object Attrs {
  val Base = Attrs(AttrLevel.Base, AttrSize.Base, AttrUsage.Base, AttrSelfUsage.Base, AttrAssumptions.Base, AttrDiverge.Base)
}

final case class Type(universe: Core, attrs: Attrs) extends Core {
  override def subst(s: Subst): Type = Type(universe.subst(s), attrs.subst(s))

  override def scan: List[Core] = List(universe) ++ attrs.scan

  override def alpha_beta_eta_equals(other: Core, map: AlphaMapping): Boolean = other match {
    case Type(otherUniverse, otherAttrs) => universe.alpha_beta_eta_equals(otherUniverse, map) && attrs.alpha_beta_eta_equals(otherAttrs, map)
    case _ => false
  }

  def validPlainSubtype(subtype: Type): Boolean = attrs.validPlainSubtype(subtype.attrs)

  def validWeakSubtype(subtype: Type): Boolean = attrs.validWeakSubtype(subtype.attrs)

  def checkPlainSubtype(subtype: Type): Maybe[Unit] = if (this.validPlainSubtype(subtype)) Right(()) else Left(ErrPlainSubtype(this, subtype))

  def checkWeakSubtype(subtype: Type): Maybe[Unit] = if (this.validWeakSubtype(subtype)) Right(()) else Left(ErrWeakSubtype(this, subtype))

  def subsetOrEqual(other: Type): Boolean = universe.alpha_beta_eta_equals(other.universe) && (attrs.alpha_beta_eta_equals(other.attrs) || attrs.merge(other.attrs).alpha_beta_eta_equals(attrs))

  def upperType: Type = Type(Cores.Universe(), attrs.upper)

  def attrsMap(f: Attrs => Attrs): Type = Type(universe, f(attrs))

  override def infer(context: Context): Maybe[Type] = Right(upperType)

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

  final case class Sigma(x: Exp, id: Var, y: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = {
      val id0 = id.gen
      Cores.Sigma(x.toCore(scope), id0, y.toCore(scope.updated(id.x, id0.x)))
    }
  }

  final case class Lambda(arg: Var, body: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = Cores.Lambda(arg.toCore(scope), body.toCore(scope))
  }

  final case class Pi(x: Exp, id: Var, y: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = {
      val id0 = id.gen
      Cores.Pi(x.toCore(scope), id0, y.toCore(scope.updated(id.x, id0.x)))
    }
  }

  final case class RecPi(x: Exp, id: Var, y: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = {
      val id0 = id.gen
      Cores.RecPi(x.toCore(scope), id0, y.toCore(scope.updated(id.x, id0.x)))
    }
  }

  final case class Rec(id: Var, kind: Exp, x: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = {
      val id0 = id.gen
      val scope0 = scope.updated(id.x, id0.x)
      Cores.Rec(id0, kind.toCore(scope0), x.toCore(scope0))
    }
  }

  final case class Recs(bindings: Set[Rec], x: Exp) extends Exp {
    if (bindings.size != bindings.toList.distinctBy(_.id).length) {
      throw new IllegalArgumentException("Recs: duplicate id")
    }

    override def toCore(scope: HashMap[Identifier, VarId]): Core = {
      val scope0 = scope ++ bindings.map(_.id).map(x => (x.x, x.gen.x))
      Cores.Recs(bindings.map(b => Cores.Rec(b.id.toCore(scope0), b.kind.toCore(scope0), b.x.toCore(scope0))), x.toCore(scope0))
    }
  }
  /*
  sealed abstract class Rec(val id: Var, val kind: Exp, val x: Exp) {
    def toCore(scope: HashMap[Identifier, VarId]): Cores.Rec
  }

  final case class RecData(override val id: Var, override val kind: Exp, override val x: Exp) extends Rec(id, kind, x) {
    override def toCore(scope: HashMap[Identifier, VarId]): Cores.Rec = Cores.RecData(id.toCore(scope), kind.toCore(scope), x.toCore(scope))
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
  */

  final case class Apply(f: Exp, x: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = Cores.Apply(f.toCore(scope), x.toCore(scope))
  }

  final case class The(t: Exp, x: Exp) extends Exp {
    override def toCore(scope: HashMap[Identifier, VarId]): Core = Cores.The(t.toCore(scope), x.toCore(scope))
  }
}

private def transverse[A](xs: List[Option[A]]): Option[List[A]] = xs match {
  case Nil => Some(Nil)
  case Some(x) :: xs => transverse(xs).map(x :: _)
  case None :: _ => None
}

object Cores {
  final case class Var(x: VarId) extends CoreNeu {
    override def scan: List[Core] = List()

    override def subst(s: Subst): Core = s.getOrElse(this, this)

    override def infer(context: Context): Maybe[Type] = context.getType(x) match {
      case Some(t) => Right(t)
      case None => Left(ErrTypeUnknown(context, this))
    }
  }

  private val NatT: Type = Type(Nat())

  final case class Zero() extends Core {
    override def scan: List[Core] = List()

    override def subst(s: Subst): Zero = this

    override def infer(context: Context): Maybe[Type] = Right(NatT)
  }

  final case class Succ(x: Core) extends Core {
    override def scan: List[Core] = List(x)

    override def subst(s: Subst): Succ = Succ(x.subst(s))

    override def infer(context: Context): Maybe[Type] = Right(NatT)
  }

  private val Universe0: Type = Type(Universe())
  private[AttrT] val UniverseInfinite: Type = Universe0.typeInType
  private val Universe1: Type = Universe0.upperType
  private val Kind0: Type = Type(Kind())
  private val KindInfinite: Type = Kind0.typeInType

  final case class Nat() extends Core with CoreType {
    override def scan: List[Core] = List()

    override def subst(s: Subst): Nat = this

    override def evalToType(context: Context): Maybe[Type] = Right(Type(this))
  }

  // type without attributes
  final case class Universe() extends Core with CoreType {
    override def scan: List[Core] = List()

    override def subst(s: Subst): Universe = this

    override def evalToType(context: Context): Maybe[Type] = Right(Universe0)
  }

  // type with attributes
  final case class Kind() extends Core with CoreType {
    override def scan: List[Core] = List()

    override def subst(s: Subst): Kind = this

    override def evalToType(context: Context): Maybe[Type] = Right(Kind0)
  }

  final case class MakeKind(x: Core) extends Core {
    override def scan: List[Core] = List(x)

    override def subst(s: Subst): MakeKind = MakeKind(x.subst(s))

    override def evalToType(context: Context): Maybe[Type] = (x.check(context, UniverseInfinite)).flatMap(_ => {
      Right(Type(x, Attrs.Base))
    })

    override def check(context: Context, t: Type): Maybe[Unit] = if (t.universe.alpha_beta_eta_equals(Kind())) {
      x.check(context, Type(Universe(), t.attrs))
    } else {
      Left(ErrCheckFailed(context, t, this, Kind()))
    }

    override def infer(context: Context): Maybe[Type] = x.check(context, UniverseInfinite).flatMap(_ => {
      x.infer(context) map {
        case Type(_, attrs) => Type(Kind(), attrs.upper)
      }
    })
  }

  final case class AttrSize(size: Core, kind: Core) extends Core {
    override def scan: List[Core] = List(size, kind)

    override def subst(s: Subst): AttrSize = AttrSize(size.subst(s), kind.subst(s))

    override def check(context: Context, t: Type): Maybe[Unit] = size.check(context, NatT) and kind.check(context, KindInfinite) and kind.check(context, t)

    override def infer(context: Context): Maybe[Type] = (size.check(context, NatT) and kind.check(context, KindInfinite)).flatMap(_ => {
      kind.infer(context) // evalToType(context).map(_.upperType)
    })

    override def evalToType(context: Context): Maybe[Type] = (size.check(context, NatT)).flatMap(_ => {
      kind.evalToType(context).map(_.sized(size))
    })
  }

  final case class Cons(x: Core, y: Core) extends Core {
    override def scan: List[Core] = List(x, y)

    override def subst(s: Subst): Cons = Cons(x.subst(s), y.subst(s))

    override def check(context: Context, t: Type): Maybe[Unit] = t.universe.reducingMatch(context, {
      case Sigma(a, id, d) => for {
        aT <- a.evalToType(context)
        _ <- t.checkPlainSubtype(aT)
        _ <- x.check(context, aT)
        innerContext = context.updated(id, aT, x)
        dT <- d.evalToType(innerContext)
        _ <- t.checkPlainSubtype(dT)
        _ <- y.check(innerContext, dT)
      } yield ()
      case wrong => Left(ErrExpected(context, "Sigma", this, wrong))
    })
  }

  final case class Car(x: Core) extends CoreNeu {
    override def scan: List[Core] = List(x)

    override def subst(s: Subst): Car = Car(x.subst(s))

    override def infer(context: Context): Maybe[Type] = x.infer(context) flatMap {
      case Type(uni, attrs) => uni.reducingMatch(context, {
        case Sigma(a, id, d) => a.evalToType(context)
        case wrong => Left(ErrExpected(context, "Sigma", x, wrong))
      })
    }
  }

  final case class Cdr(x: Core) extends CoreNeu {
    override def scan: List[Core] = List(x)

    override def subst(s: Subst): Cdr = Cdr(x.subst(s))

    override def infer(context: Context): Maybe[Type] = x.infer(context) flatMap {
      case Type(uni, attrs) => uni.reducingMatch(context, {
        case Sigma(a, id, d) => a.evalToType(context).flatMap((at) => d.evalToType(context.updated(id, at)))
        case wrong => Left(ErrExpected(context, "Sigma", x, wrong))
      })
    }
  }

  final case class Sigma(x: Core, id: Var, y: Core) extends Core with CoreType {
    override def scan: List[Core] = List(x, y)

    override def subst(s: Subst): Sigma = Sigma(x.subst(s), id, y.subst(s))

    override def evalToType(context: Context): Maybe[Type] = Right(Type(this))
  }

  final case class Lambda(arg: Var, body: Core) extends Core {
    override def scan: List[Core] = List(body)

    override def subst(s: Subst): Lambda = Lambda(arg, body.subst(s))

    override def check(context: Context, t: Type): Maybe[Unit] = t.universe.reducingMatch(context, {
      case Pi(arg0, id, result0) => for {
        argT <- arg0.evalToType(context)
        _ <- t.checkWeakSubtype(argT)
        innerContext = context.updated(arg, argT).updated(id, argT, arg)
        resultT <- result0.evalToType(innerContext)
        _ <- t.checkPlainSubtype(resultT)
        _ <- body.check(innerContext, resultT)
      } yield ()
      case wrong => Left(ErrExpected(context, "Pi", this, wrong))
    })

    def checkWithRecSize(context: Context, t: Type, recSize: Core): Maybe[Unit] = ???
  }

  final case class Pi(x: Core, id: Var, y: Core) extends Core with CoreType {
    override def scan: List[Core] = List(x, y)

    override def subst(s: Subst): Pi = Pi(x.subst(s), id, y.subst(s))

    override def evalToType(context: Context): Maybe[Type] = Right(Type(this))
  }

  final case class RecPi(x: Core, id: Var, y: Core) extends Core with CoreType {
    override def scan: List[Core] = List(x, y)

    override def subst(s: Subst): Pi = Pi(x.subst(s), id, y.subst(s))

    override def evalToType(context: Context): Maybe[Type] = Right(Type(this))
  }

  final case class Rec(id: Var, kind: Core, x: Core) extends Core {
    override def scan: List[Core] = List(x)

    override def subst(s: Subst): Rec = Rec(id, kind.subst(s), x.subst(s))

    private def recs = Recs(Set(this), id)

    override def check(context: Context, t: Type): Maybe[Unit] = recs.check(context, t)

    override def infer(context: Context): Maybe[Type] = recs.infer(context)

    override def reduce(context: Context): Core = recs.reduce(context)
  }

  final case class Recs(bindings: Set[Rec], x: Core) extends Core {
    if (bindings.size != bindings.toList.distinctBy(_.id).length) {
      throw new IllegalArgumentException("Recs: duplicate id")
    }

    override def reduce(context: Context): Core = ???

    override def scan: List[Core] = bindings.toList ++ List(x)

    override def subst(s: Subst): Recs = Recs(bindings.map(_.subst(s)), x.subst(s))

    private def checkBindings(context: Context): Maybe[Context] = {
      val innerContext0 = context.concat(bindings.toList.map(Recs.checkRec(context, _)).map(_.toOption).flatten)

      def step(stepContext: Context) = context.concat(bindings.toList.map(Recs.checkRec(stepContext, _)).map(_.toOption).flatten)

      val innerContext = step(step(step(step(step(step(step(step(step(step(step(step(step(step(step(step(innerContext0))))))))))))))))
      val addition0 = bindings.toList.map(Recs.checkRec(innerContext, _))
      val addition = addition0.map(_.toOption).flatten
      val failed = addition0.collect {
        case Left(e) => e
      }
      if (failed.isEmpty) {
        if (addition.length != bindings.size) {
          throw new IllegalStateException("addition.length!=bindings.size")
        }
        Right(context.concat(addition))
      } else {
        Left(ErrRecs(context, failed))
      }
    }

    override def check(context: Context, t: Type): Maybe[Unit] = for {
      ctx <- checkBindings(context)
      _ <- x.check(ctx, t)
    } yield ()

    override def infer(context: Context): Maybe[Type] = for {
      ctx <- checkBindings(context)
      t <- x.infer(ctx)
    } yield t
  }

  object Recs {
    private def extract(context: Context, x: Core): List[Core] = x match {
      case v: Var => context.getValue(v).toList
      case other => other.scan
    }

    private def coreContains(context: Context, element: Core, history: Set[Core], current: Core): Boolean = {
      if (element == current) {
        true
      } else if (history.contains(current)) {
        false
      } else {
        val newHistory = history.incl(current)
        extract(context, current).exists(coreContains(context, element, newHistory, _))
      }
    }

    private def isRecursive(context: Context, x: Core): Boolean = extract(context, x).exists(coreContains(context, x, Set(), _))

    private def checkRec(context: Context, rec: Rec): Maybe[(VarId, Type, Core)] = for {
      kind <- rec.kind.evalToType(context)
      _ <- checkRec(context, rec.id, kind, rec.x)
    } yield (rec.id.x, kind, rec.x)

    private def checkRec(context: Context, id: Var, kind: Type, x: Core): Maybe[Unit] = x.check(context, kind).flatMap(_ => if (isRecursive(context, x)) {
      if (kind.attrs.size == AttrSize_UnknownFinite()) {
        Left(ErrUnknownFiniteRec(context, id, x, kind))
        // other parts will handle finite and infinite correctly
      } else {
        kind.universe.weakHeadNormalForm(context) match {
          case Pi(arg, argId, result) => for {
            argK <- arg.evalToType(context)
            resultK <- result.evalToType(context.updated(argId, argK))
            _ <- if (resultK.attrs.diverge == AttrDiverge_Yes()) Right(()) else Left(ErrNotDivergePiRec(context, id, x, kind))
          } yield ()
          case _: CoreNeu => Left(ErrUnknownTypeRec(context, id, x, kind))
          case _: RecPi => Right(())
          case _ => Right(())
        }
      }
    } else {
      Right(())
    })
  }

  /*
  sealed abstract class Rec(val id: Var, val kind: Core, val x: Core) {
    def scanPlain(v: Cores.Var): NaturalNumber = x.scanVar(v)

    def subst(s: Subst): Rec
  }

  final case class RecData(override val id: Var, override val kind: Core, override val x: Core) extends Rec(id, kind, x) {
    override def subst(s: Subst): RecData = RecData(id, kind.subst(s), x.subst(s))
  }

  final case class RecPi(override val id: Var, override val kind: Core, override val x: Core) extends Rec(id, kind, x) {
    override def subst(s: Subst): RecPi = RecPi(id, kind.subst(s), x.subst(s))
  }

  final case class Letrec(bindings: Set[Rec], x: Core) extends Core {
    override def scanVar(v: Cores.Var): NaturalNumber = bindings.map(_.scanPlain(v)).sum + x.scanVar(v)

    if (bindings.size != bindings.toList.distinctBy(_.id).length) {
      throw new Error("letrec: duplicate id")
    }

    override def subst(s: Subst): Letrec = Letrec(bindings.map(_.subst(s)), x.subst(s))

    private def checkBindings(context: Context): Maybe[Context] = {
      val innerContext0 = context.concat(bindings.toList.map(x => x.kind.evalToType.map(t => (x.id.x, t, x.x))).map(_.toOption).flatten)

      def step(stepContext: Context) = context.concat(bindings.toList.map(x => x.kind.evalToType(stepContext).map(t => (x.id.x, t, x.x))).map(_.toOption).flatten)

      val innerContext = step(step(step(step(step(step(step(step(step(step(step(step(step(step(step(step(innerContext0))))))))))))))))

      def check(stepContext: Context): Maybe[Context] = bindings.toList.foldLeft(Right(stepContext): Maybe[Context])((c, rec) => c.flatMap(ctx => Letrec.checkBinding(ctx, rec, this)))

      for {
        ctx1 <- check(innerContext)
        _ <- check(ctx1)
      } yield innerContext
    }

    override def check(context: Context, t: Type): Maybe[Unit] = this.checkBindings(context) match {
      case Right(innerContext) => x.check(innerContext, t)
      case Left(err) => Left(err)
    }
  }

  object Letrec {
    private def scanIfRec(bindings: HashMap[Var, Rec], id: Var, history: Set[Core], current: Core): Boolean =
      if (history.contains(current)) {
        false
      } else {
        if (!bindings.forall((v, r) => v == r.id)) {
          throw new IllegalArgumentException("Illegal bindings")
        }
        val nextScan = bindings.keySet.filterNot(current.scanVar(_) == 0)
        if (nextScan.contains(id)) {
          true
        } else {
          nextScan.exists(v => {
            val next = bindings.get(v).get.x
            scanIfRec(bindings, id, history.incl(next), next)
          })
        }
      }

    private def checkIfInfinite(bindings: Set[Rec], bind: Rec): Boolean = if (!bindings.contains(bind)) {
      throw new IllegalArgumentException("bindings must contain bind")
    } else {
      scanIfRec(HashMap().concat(bindings.map(r => (r.id, r))), bind.id, Set(), bind.x)
    }

    private def checkBinding(context: Context, bind: Rec, letrec: Letrec): Maybe[Context] = bind.kind.evalToType(context).flatMap(bindType => {
      bind.x.check(context, bindType).flatMap(_ => {
        bind match {
          case bind@RecData(id, _, _) => if (bindType.attrs.size == AttrSize_Infinite()) {
            Right(context)
          } else if (checkIfInfinite(letrec.bindings, bind)) {
            Left(ErrExpectedCodata(context, bind, bindType))
          } else {
            Right(context.addRec(id))
          }
          case RecPi(id, _, bindBody) => bindBody.reducingMatch(context, {
            case lambda: Lambda =>
              bindType.universe.reducingMatch(context, {
                case Pi(arg, argId, result) => arg.evalToType(context) match {
                  case Right(argType@Type(_, argAttrs)) => {
                    val doesRec = checkIfInfinite(letrec.bindings, bind)
                    if (doesRec) {
                      val resultContext = context.updated(argId, argType)

                      val resultDiverge = result.evalToType(resultContext) match {
                        case Right(Type(_, resultAttrs)) => resultAttrs.diverge == AttrDiverge_Yes()
                        case Left(_) => false
                      }

                      argAttrs.size match {
                        case AttrSize_Infinite() | AttrSize_UnknownFinite() => if (resultDiverge) Right(context) else Left(ErrDiverge(context, bind))
                        case AttrSize_Known(size) => if (resultDiverge) Right(context) else lambda.checkWithRecSize(context, bindType, size).flatMap(_ => Right(context.addRecPi(id)))
                      }
                    } else {
                      Right(context)
                    }
                  }
                  case Left(err) => Left(err)
                }
                case wrong => Left(ErrExpected(context, "Pi", bindBody, wrong))
              })
            case wrong => Left(ErrExpected(context, "Lambda", bindBody, wrong))
          })
        }
      })
    })
  }
  */

  final case class Apply(f: Core, x: Core) extends CoreNeu {
    override def scan: List[Core] = List(f, x)

    override def subst(s: Subst): Apply = Apply(f.subst(s), x.subst(s))

    override def reduce(context: Context): Core = f.infer(context).flatMap(t => t.universe.reducingMatch(context, {
      case Pi(argT, tid, resultT) => f.reducingMatch(context, {
        case Lambda(arg, body) => argT.evalToType(context).flatMap(argK => Right(body.subst(arg, argK, x)))
        case wrong => Left(ErrExpectedV(context, "Pi", wrong))
      })
      case wrong => Left(ErrExpected(context, "Pi", f, wrong))
    })) getOrElse this

    override def infer(context: Context): Maybe[Type] = f.infer(context).flatMap(t => t.universe.reducingMatch(context, {
      case Pi(argT, tid, resultT) => for {
        argK <- argT.evalToType(context)
        _ <- x.check(context, argK)
        resultK <- resultT.evalToType(context.updated(tid, argK, x))
      } yield resultK
      case wrong => Left(ErrExpected(context, "Pi", f, wrong))
    }))
  }

  final case class The(t: Core, x: Core) extends Core {
    override def scan: List[Core] = List(x)

    override def subst(s: Subst): The = The(t.subst(s), x.subst(s))

    override def infer(context: Context): Maybe[Type] = t.evalToType(context)

    override def reduce(context: Context): Core = t.evalToType(context).map(InternalThe(_, x)) getOrElse this
  }

  final case class InternalThe(t: Type, x: Core) extends Core {
    override def scan: List[Core] = List(x)

    override def subst(s: Subst): InternalThe = InternalThe(t.subst(s), x.subst(s))

    override def infer(context: Context): Maybe[Type] = Right(t)

    override def reduce(context: Context): Core = x // x.check(context, t).map(_ => x) getOrElse this
  }
}