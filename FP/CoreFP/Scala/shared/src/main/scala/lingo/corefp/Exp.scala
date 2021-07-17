package lingo.corefp

import lingo.corefp.utils.Nat

sealed abstract class Exp(name: Atom, xs: List[Value]) {
  def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value

  def toValue: Value = TaggedSeq(Atoms.Exp, name, ValueList(xs))
}

final case class GeneralExp(name: Atom, xs: List[Value])

object ValueExp extends CachedValueT[Exp] {
  val ValueListExp = ValueListT(ValueExp)
  override val helper = Helper()

  override def internal_apply(x: Exp): Value = x.toValue

  override def internal_unapply(x: Value): Option[Exp] = x match {
    case TaggedSeq(Atoms.Exp, name: Atom, ValueList(xs)) => GeneralExp(name, xs) match {
      case ExpExtractorQuote(v) => Some(v)
      case ExpExtractorCommented(v) => Some(v)
      case ExpExtractorLocated(v) => Some(v)
      case ExpExtractorApplyFunction(v) => Some(v)
      case ExpExtractorApplyMacro(v) => Some(v)
      case ExpExtractorVar(v) => Some(v)
      case ExpExtractorFunction(v) => Some(v)
      case ExpExtractorRecursive(v) => Some(v)
      case ExpExtractorBuiltin(v) => Some(v)
      case _ => None
    }
    case _ => None
  }
}

trait ExpExtractorT[T <: Exp] {
  def unapply(x: GeneralExp): Option[T]
}

final case class Quote(x: Value) extends Exp(Atoms.Exps.Quote, List(x)) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = x
}

object ExpExtractorQuote extends ExpExtractorT[Quote] {
  override def unapply(x: GeneralExp): Option[Quote] = x match {
    case GeneralExp(Atoms.Exps.Quote, List(x)) => Some(Quote(x))
    case _ => None
  }
}

final case class Commented(comment: Value, x: Exp) extends Exp(Atoms.Exps.Commented, List(comment, ValueExp(x))) {
  def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = x.eval
}

object ExpExtractorCommented extends ExpExtractorT[Commented] {
  override def unapply(x: GeneralExp): Option[Commented] = x match {
    case GeneralExp(Atoms.Exps.Commented, List(comment, ValueExp(x))) => Some(Commented(comment, x))
    case _ => None
  }
}

// todo
sealed trait Location

sealed trait MaybeDebugStack {
  def updated(x: Location): MaybeDebugStack
}

final case class DebugStack(xs: List[Location]) extends MaybeDebugStack {
  override def updated(x: Location): DebugStack = DebugStack(x :: xs)
}

case object NotDebugStack extends MaybeDebugStack {
  override def updated(x: Location): NotDebugStack.type = NotDebugStack
}

object Trivial {
  private val instance: Value = TaggedSeq(Atoms.Tags.Trivial)

  def apply(): Value = instance

  def unapply(x: Value): Boolean = x match {
    case TaggedSeq(Atoms.Tags.Trivial) => true
    case _ => false
  }

}

object ValueLocation extends CachedValueT[Location] {
  override val helper = Helper()

  override def internal_apply(x: Location): Value = x match {
    case UNIXFileLocation(file, location, Some(name)) => TaggedSeq(Atoms.Tags.UNIXFilePosition, ValueString(file), ValueNat(location), ValueString(name))
    case UNIXFileLocation(file, location, None) => TaggedSeq(Atoms.Tags.UNIXFilePosition, ValueString(file), ValueNat(location), Trivial())
  }

  override def internal_unapply(x: Value): Option[Location] = x match {
    case TaggedSeq(Atoms.Tags.UNIXFilePosition, ValueString(file), ValueNat(location), ValueString(name)) => Some(UNIXFileLocation(file, location, Some(name)))
    case TaggedSeq(Atoms.Tags.UNIXFilePosition, ValueString(file), ValueNat(location), Trivial()) => Some(UNIXFileLocation(file, location, None))
    case _ => None
  }
}

final case class UNIXFileLocation(file: String, location: Nat, name: Option[String]) extends Location

final case class Located(location: Location, x: Exp) extends Exp(Atoms.Exps.Located, List(ValueLocation(location), ValueExp(x))) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = x.eval(env, debugStack.updated(location))
}

object ExpExtractorLocated extends ExpExtractorT[Located] {
  override def unapply(x: GeneralExp): Option[Located] = x match {
    case GeneralExp(Atoms.Exps.Located, List(ValueLocation(location), ValueExp(x))) => Some(Located(location, x))
    case _ => None
  }
}

final case class ApplyFunction(f: Exp, xs: List[Exp]) extends Exp(Atoms.ApplyFunction, List(ValueExp(f), ValueExp.ValueListExp(xs))) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = f.eval match {
    case ValueClosure(c) => c.apply(xs.map(_.eval)).getOrElse({
      Builtin.evalException(this, Atoms.ExceptionReasons.ArgsMismatch)
    })
    case _ => Builtin.evalException(this, Atoms.ExceptionReasons.TypeMismatch)
  }
}

object ExpExtractorApplyFunction extends ExpExtractorT[ApplyFunction] {
  override def unapply(x: GeneralExp): Option[ApplyFunction] = x match {
    case GeneralExp(Atoms.ApplyFunction, List(ValueExp(f), ValueExp.ValueListExp(xs))) => Some(ApplyFunction(f, xs))
    case _ => None
  }
}

final case class ApplyMacro(m: Exp, xs: List[Exp]) extends Exp(Atoms.Exps.ApplyMacro, List(ValueExp(m), ValueExp.ValueListExp(xs))) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = m.eval match {
    case ValueMacro(Macro(mc)) => mc.apply(ValueHashMap(env) :: xs.map(_.eval)).getOrElse({
      Builtin.evalException(this, Atoms.ExceptionReasons.ArgsMismatch)
    })
    case _ => Builtin.evalException(this, Atoms.ExceptionReasons.TypeMismatch)
  }
}

object ExpExtractorApplyMacro extends ExpExtractorT[ApplyMacro] {
  override def unapply(x: GeneralExp): Option[ApplyMacro] = x match {
    case GeneralExp(Atoms.Exps.ApplyMacro, List(ValueExp(m), ValueExp.ValueListExp(xs))) => Some(ApplyMacro(m, xs))
    case _ => None
  }
}

final case class Var(id: Value) extends Exp(Atoms.Exps.Var, List(id)) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = env.getOrElse(id, {
    todo()
  })
}

object ExpExtractorVar extends ExpExtractorT[Var] {
  override def unapply(x: GeneralExp): Option[Var] = x match {
    case GeneralExp(Atoms.Exps.Var, List(id)) => Some(Var(id))
    case _ => None
  }
}

object ValueVar extends CachedValueT[Var] {
  override protected val helper = Helper()

  override protected def internal_apply(x: Var): Value = x.toValue

  override protected def internal_unapply(x: Value): Option[Var] = x match {
    case ValueExp(x: Var) => Some(x)
    case _ => None
  }
}

final case class Function(args: Args, body: Exp) extends Exp(Atoms.Func, List(ValueArgs(args), ValueExp(body))) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = ValueClosure(Closure(env, args, body))
}

object ExpExtractorFunction extends ExpExtractorT[Function] {
  override def unapply(x: GeneralExp): Option[Function] = x match {
    case GeneralExp(Atoms.Func, List(ValueArgs(args), ValueExp(body))) => Some(Function(args, body))
    case _ => None
  }
}

final case class Recursive(self: Var, body: Exp) extends Exp(Atoms.Exps.Recursive, List(ValueVar(self), ValueExp(body))) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = {
    lazy val result: PossiblyRecursive = new PossiblyRecursive({
      body.eval(env.updated(self.id, result), implicitly)
    })
    result
  }
}

object ExpExtractorRecursive extends ExpExtractorT[Recursive] {
  override def unapply(x: GeneralExp): Option[Recursive] = x match {
    case GeneralExp(Atoms.Exps.Recursive, List(ValueVar(self), ValueExp(body))) => Some(Recursive(self, body))
    case _ => None
  }
}

sealed abstract class Builtin(name: Atom, xs: List[Exp]) extends Exp(Atoms.Builtin, List(name, ValueExp.ValueListExp(xs))) {
  protected final def exception(reason: Atom)(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value =
    Builtin.exception(this.name, this.xs, reason)(env, debugStack)
}

object Builtin {
  // todo debugstack
  private[corefp] def exception(name: Atom, xs: List[Exp], reason: Atom)(implicit env: ValueHashMap.Type, _debugStack: MaybeDebugStack): Value = ExceptionSeq(Atoms.Builtin, reason, name, ValueExp.ValueListExp(xs), ValueHashMap(env))

  private[corefp] def evalException(x: Exp, reason: Atom)(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = Builtin.exception(Atoms.Builtins.Eval, List(Quote(ValueHashMap(env)), Quote(ValueExp(x))), reason)
}

final case class GeneralBuiltin(name: Atom, xs: List[Exp])

trait BuiltinExtractorT[T <: Exp] {
  def unapply(x: GeneralBuiltin): Option[T]
}

object ExpExtractorBuiltin extends ExpExtractorT[Builtin] {
  override def unapply(x: GeneralExp): Option[Builtin] = x match {
    case GeneralExp(Atoms.Builtin, List(name: Atom, ValueExp.ValueListExp(xs))) => GeneralBuiltin(name, xs) match {
      case BuiltinExtractorIsAtom(x) => Some(x)
      case BuiltinExtractorIsEmptyList(x) => Some(x)
      case BuiltinExtractorIsNonEmptyList(x) => Some(x)
      case BuiltinExtractorIsTagged(x) => Some(x)
      case BuiltinExtractorIsException(x) => Some(x)
      case BuiltinExtractorIsResource(x) => Some(x)
      case BuiltinExtractorIntroNonEmptyList(x) => Some(x)
      case BuiltinExtractorElimNonEmptyListHead(x) => Some(x)
      case BuiltinExtractorElimNonEmptyListTail(x) => Some(x)
      case BuiltinExtractorIntroTagged(x) => Some(x)
      case BuiltinExtractorElimTaggedTag(x) => Some(x)
      case BuiltinExtractorElimTaggedData(x) => Some(x)
      case BuiltinExtractorIntroException(x) => Some(x)
      case BuiltinExtractorElimExceptionTag(x) => Some(x)
      case BuiltinExtractorElimExceptionData(x) => Some(x)
      case BuiltinExtractorElimResourceTag(x) => Some(x)
      case BuiltinExtractorElimResourceData(x) => Some(x)
      case BuiltinExtractorElimBoolean(x) => Some(x)
      case BuiltinExtractorEqual(x) => Some(x)
      case BuiltinExtractorBuiltinEval(x) => Some(x)
      case BuiltinExtractorBuiltinApplyFunction(x) => Some(x)
      case _ => None
    }
    case _ => None
  }
}

sealed abstract class BuiltinFunctionUnary(name: Atom, x: Exp) extends Builtin(name, List(x)) {
}

final case class GeneralBuiltinFunctionUnary(name: Atom, x: Exp)

trait BuiltinFunctionUnaryExtractorT[T <: BuiltinFunctionUnary] extends BuiltinExtractorT[T] {
  def unapply(x: GeneralBuiltinFunctionUnary): Option[T]

  final override def unapply(x: GeneralBuiltin): Option[T] = x match {
    case GeneralBuiltin(name, List(x)) => unapply(GeneralBuiltinFunctionUnary(name, x))
    case _ => None
  }
}

sealed abstract class BuiltinFunctionBinary(name: Atom, x: Exp, y: Exp) extends Builtin(name, List(x, y)) {
}

final case class GeneralBuiltinFunctionBinary(name: Atom, x: Exp, y: Exp)

trait BuiltinFunctionBinaryExtractorT[T <: BuiltinFunctionBinary] extends BuiltinExtractorT[T] {
  def unapply(x: GeneralBuiltinFunctionBinary): Option[T]

  final override def unapply(x: GeneralBuiltin): Option[T] = x match {
    case GeneralBuiltin(name, List(x, y)) => unapply(GeneralBuiltinFunctionBinary(name, x, y))
    case _ => None
  }
}

sealed abstract class BuiltinFunctionTriple(name: Atom, x: Exp, y: Exp, z: Exp) extends Builtin(name, List(x, y, z)) {
}

final case class GeneralBuiltinFunctionTriple(name: Atom, x: Exp, y: Exp, z: Exp)

trait BuiltinFunctionTripleExtractorT[T <: BuiltinFunctionTriple] extends BuiltinExtractorT[T] {
  def unapply(x: GeneralBuiltinFunctionTriple): Option[T]

  final override def unapply(x: GeneralBuiltin): Option[T] = x match {
    case GeneralBuiltin(name, List(x, y, z)) => unapply(GeneralBuiltinFunctionTriple(name, x, y, z))
    case _ => None
  }
}

final case class IsAtom(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.IsAtom, x) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = x.eval match {
    case Atom(_) => ValueBoolean.True
    case _ => ValueBoolean.False
  }
}

object BuiltinExtractorIsAtom extends BuiltinFunctionUnaryExtractorT[IsAtom] {
  override def unapply(x: GeneralBuiltinFunctionUnary): Option[IsAtom] = x match {
    case GeneralBuiltinFunctionUnary(Atoms.Builtins.IsAtom, x) => Some(IsAtom(x))
    case _ => None
  }
}

final case class IsEmptyList(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.IsEmptyList, x) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = x.eval match {
    case EmptyList() => ValueBoolean.True
    case _ => ValueBoolean.False
  }
}

object BuiltinExtractorIsEmptyList extends BuiltinFunctionUnaryExtractorT[IsEmptyList] {
  override def unapply(x: GeneralBuiltinFunctionUnary): Option[IsEmptyList] = x match {
    case GeneralBuiltinFunctionUnary(Atoms.Builtins.IsEmptyList, x) => Some(IsEmptyList(x))
    case _ => None
  }
}

final case class IsNonEmptyList(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.IsNonEmptyList, x) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = x.eval match {
    case NonEmptyList(_, _) => ValueBoolean.True
    case _ => ValueBoolean.False
  }
}

object BuiltinExtractorIsNonEmptyList extends BuiltinFunctionUnaryExtractorT[IsNonEmptyList] {
  override def unapply(x: GeneralBuiltinFunctionUnary): Option[IsNonEmptyList] = x match {
    case GeneralBuiltinFunctionUnary(Atoms.Builtins.IsNonEmptyList, x) => Some(IsNonEmptyList(x))
    case _ => None
  }
}

final case class IsTagged(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.IsTagged, x) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = x.eval match {
    case Tagged(_, _) => ValueBoolean.True
    case _ => ValueBoolean.False
  }
}

object BuiltinExtractorIsTagged extends BuiltinFunctionUnaryExtractorT[IsTagged] {
  override def unapply(x: GeneralBuiltinFunctionUnary): Option[IsTagged] = x match {
    case GeneralBuiltinFunctionUnary(Atoms.Builtins.IsTagged, x) => Some(IsTagged(x))
    case _ => None
  }
}

final case class IsException(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.IsException, x) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = x.eval match {
    case Exception(_, _) => ValueBoolean.True
    case _ => ValueBoolean.False
  }
}

object BuiltinExtractorIsException extends BuiltinFunctionUnaryExtractorT[IsException] {
  override def unapply(x: GeneralBuiltinFunctionUnary): Option[IsException] = x match {
    case GeneralBuiltinFunctionUnary(Atoms.Builtins.IsException, x) => Some(IsException(x))
    case _ => None
  }
}

final case class IsResource(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.IsResource, x) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = x.eval match {
    case Resource(_, _, _, _) => ValueBoolean.True
    case _ => ValueBoolean.False
  }
}

object BuiltinExtractorIsResource extends BuiltinFunctionUnaryExtractorT[IsResource] {
  override def unapply(x: GeneralBuiltinFunctionUnary): Option[IsResource] = x match {
    case GeneralBuiltinFunctionUnary(Atoms.Builtins.IsResource, x) => Some(IsResource(x))
    case _ => None
  }
}

final case class IntroNonEmptyList(x: Exp, y: Exp) extends BuiltinFunctionBinary(Atoms.Builtins.IntroNonEmptyList, x, y) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = NonEmptyList(x.eval, y.eval)
}

object BuiltinExtractorIntroNonEmptyList extends BuiltinFunctionBinaryExtractorT[IntroNonEmptyList] {
  override def unapply(x: GeneralBuiltinFunctionBinary): Option[IntroNonEmptyList] = x match {
    case GeneralBuiltinFunctionBinary(Atoms.Builtins.IntroNonEmptyList, x, y) => Some(IntroNonEmptyList(x, y))
    case _ => None
  }
}

final case class ElimNonEmptyListHead(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.ElimNonEmptyListHead, x) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = x.eval match {
    case NonEmptyList(a, _) => a
    case _ => this.exception(Atoms.ExceptionReasons.TypeMismatch)
  }
}

object BuiltinExtractorElimNonEmptyListHead extends BuiltinFunctionUnaryExtractorT[ElimNonEmptyListHead] {
  override def unapply(x: GeneralBuiltinFunctionUnary): Option[ElimNonEmptyListHead] = x match {
    case GeneralBuiltinFunctionUnary(Atoms.Builtins.ElimNonEmptyListHead, x) => Some(ElimNonEmptyListHead(x))
    case _ => None
  }
}

final case class ElimNonEmptyListTail(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.ElimNonEmptyListTail, x) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = x.eval match {
    case NonEmptyList(_, b) => b
    case _ => this.exception(Atoms.ExceptionReasons.TypeMismatch)
  }
}

object BuiltinExtractorElimNonEmptyListTail extends BuiltinFunctionUnaryExtractorT[ElimNonEmptyListTail] {
  override def unapply(x: GeneralBuiltinFunctionUnary): Option[ElimNonEmptyListTail] = x match {
    case GeneralBuiltinFunctionUnary(Atoms.Builtins.ElimNonEmptyListTail, x) => Some(ElimNonEmptyListTail(x))
    case _ => None
  }
}

final case class IntroTagged(x: Exp, y: Exp) extends BuiltinFunctionBinary(Atoms.Builtins.IntroTagged, x, y) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = Tagged(x.eval, y.eval)
}

object BuiltinExtractorIntroTagged extends BuiltinFunctionBinaryExtractorT[IntroTagged] {
  override def unapply(x: GeneralBuiltinFunctionBinary): Option[IntroTagged] = x match {
    case GeneralBuiltinFunctionBinary(Atoms.Builtins.IntroTagged, x, y) => Some(IntroTagged(x, y))
    case _ => None
  }
}

final case class ElimTaggedTag(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.ElimTaggedTag, x) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = x.eval match {
    case Tagged(a, _) => a
    case _ => this.exception(Atoms.ExceptionReasons.TypeMismatch)
  }
}

object BuiltinExtractorElimTaggedTag extends BuiltinFunctionUnaryExtractorT[ElimTaggedTag] {
  override def unapply(x: GeneralBuiltinFunctionUnary): Option[ElimTaggedTag] = x match {
    case GeneralBuiltinFunctionUnary(Atoms.Builtins.ElimTaggedTag, x) => Some(ElimTaggedTag(x))
    case _ => None
  }
}

final case class ElimTaggedData(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.ElimTaggedData, x) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = x.eval match {
    case Tagged(_, b) => b
    case _ => this.exception(Atoms.ExceptionReasons.TypeMismatch)
  }
}

object BuiltinExtractorElimTaggedData extends BuiltinFunctionUnaryExtractorT[ElimTaggedData] {
  override def unapply(x: GeneralBuiltinFunctionUnary): Option[ElimTaggedData] = x match {
    case GeneralBuiltinFunctionUnary(Atoms.Builtins.ElimTaggedData, x) => Some(ElimTaggedData(x))
    case _ => None
  }
}

final case class IntroException(x: Exp, y: Exp) extends BuiltinFunctionBinary(Atoms.Builtins.IntroException, x, y) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = Exception(x.eval, y.eval)
}

object BuiltinExtractorIntroException extends BuiltinFunctionBinaryExtractorT[IntroException] {
  override def unapply(x: GeneralBuiltinFunctionBinary): Option[IntroException] = x match {
    case GeneralBuiltinFunctionBinary(Atoms.Builtins.IntroException, x, y) => Some(IntroException(x, y))
    case _ => None
  }
}

final case class ElimExceptionTag(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.ElimExceptionTag, x) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = x.eval match {
    case Exception(a, _) => a
    case _ => this.exception(Atoms.ExceptionReasons.TypeMismatch)
  }
}

object BuiltinExtractorElimExceptionTag extends BuiltinFunctionUnaryExtractorT[ElimExceptionTag] {
  override def unapply(x: GeneralBuiltinFunctionUnary): Option[ElimExceptionTag] = x match {
    case GeneralBuiltinFunctionUnary(Atoms.Builtins.ElimExceptionTag, x) => Some(ElimExceptionTag(x))
    case _ => None
  }
}

final case class ElimExceptionData(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.ElimExceptionData, x) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = x.eval match {
    case Exception(_, b) => b
    case _ => this.exception(Atoms.ExceptionReasons.TypeMismatch)
  }
}

object BuiltinExtractorElimExceptionData extends BuiltinFunctionUnaryExtractorT[ElimExceptionData] {
  override def unapply(x: GeneralBuiltinFunctionUnary): Option[ElimExceptionData] = x match {
    case GeneralBuiltinFunctionUnary(Atoms.Builtins.ElimExceptionData, x) => Some(ElimExceptionData(x))
    case _ => None
  }
}

final case class ElimResourceTag(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.ElimResourceTag, x) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = x.eval match {
    case Resource(a, _, _, _) => a
    case _ => this.exception(Atoms.ExceptionReasons.TypeMismatch)
  }
}

object BuiltinExtractorElimResourceTag extends BuiltinFunctionUnaryExtractorT[ElimResourceTag] {
  override def unapply(x: GeneralBuiltinFunctionUnary): Option[ElimResourceTag] = x match {
    case GeneralBuiltinFunctionUnary(Atoms.Builtins.ElimResourceTag, x) => Some(ElimResourceTag(x))
    case _ => None
  }
}

final case class ElimResourceData(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.ElimResourceData, x) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = x.eval match {
    case Resource(_, b, _, _) => b
    case _ => this.exception(Atoms.ExceptionReasons.TypeMismatch)
  }
}

object BuiltinExtractorElimResourceData extends BuiltinFunctionUnaryExtractorT[ElimResourceData] {
  override def unapply(x: GeneralBuiltinFunctionUnary): Option[ElimResourceData] = x match {
    case GeneralBuiltinFunctionUnary(Atoms.Builtins.ElimResourceData, x) => Some(ElimResourceData(x))
    case _ => None
  }
}

// Yeah ... ElimBoolean is a function in some models
final case class ElimBoolean(x: Exp, a: Exp, b: Exp) extends BuiltinFunctionTriple(Atoms.Builtins.ElimBoolean, x, a, b) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = x.eval match {
    case ValueBoolean(x0) => (if (x0) {
      a
    } else {
      b
    }).eval
    case _ => this.exception(Atoms.ExceptionReasons.TypeMismatch)
  }
}

object BuiltinExtractorElimBoolean extends BuiltinFunctionTripleExtractorT[ElimBoolean] {
  override def unapply(x: GeneralBuiltinFunctionTriple): Option[ElimBoolean] = x match {
    case GeneralBuiltinFunctionTriple(Atoms.Builtins.ElimBoolean, x, a, b) => Some(ElimBoolean(x, a, b))
    case _ => None
  }
}

final case class Equal(x: Exp, y: Exp) extends BuiltinFunctionBinary(Atoms.Builtins.Equal, x, y) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = ValueBoolean(x.eval equals y.eval)
}

object BuiltinExtractorEqual extends BuiltinFunctionBinaryExtractorT[Equal] {
  override def unapply(x: GeneralBuiltinFunctionBinary): Option[Equal] = x match {
    case GeneralBuiltinFunctionBinary(Atoms.Builtins.Equal, x, y) => Some(Equal(x, y))
    case _ => None
  }
}

final case class BuiltinEval(x: Exp, y: Exp) extends BuiltinFunctionBinary(Atoms.Builtins.Eval, x, y) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = (x.eval, y.eval) match {
    case (ValueHashMap(env), ValueExp(e)) => e.eval(env, implicitly)
    case _ => exception(Atoms.ExceptionReasons.IllegalExp)
  }
}

object BuiltinExtractorBuiltinEval extends BuiltinFunctionBinaryExtractorT[BuiltinEval] {
  override def unapply(x: GeneralBuiltinFunctionBinary): Option[BuiltinEval] = x match {
    case GeneralBuiltinFunctionBinary(Atoms.Builtins.Eval, x, y) => Some(BuiltinEval(x, y))
    case _ => None
  }
}

final case class BuiltinApplyFunction(x: Exp, y: Exp) extends BuiltinFunctionBinary(Atoms.ApplyFunction, x, y) {
  override def eval(implicit env: ValueHashMap.Type, debugStack: MaybeDebugStack): Value = x.eval match {
    case ValueClosure(c) => y.eval match {
      case ValueList(xs) => c.apply(xs).getOrElse({
        exception(Atoms.ExceptionReasons.ArgsMismatch)
      })
      case _ => exception(Atoms.ExceptionReasons.TypeMismatch)
    }
    case _ => exception(Atoms.ExceptionReasons.TypeMismatch)
  }
}

object BuiltinExtractorBuiltinApplyFunction extends BuiltinFunctionBinaryExtractorT[BuiltinApplyFunction] {
  override def unapply(x: GeneralBuiltinFunctionBinary): Option[BuiltinApplyFunction] = x match {
    case GeneralBuiltinFunctionBinary(Atoms.Builtins.Eval, x, y) => Some(BuiltinApplyFunction(x, y))
    case _ => None
  }
}