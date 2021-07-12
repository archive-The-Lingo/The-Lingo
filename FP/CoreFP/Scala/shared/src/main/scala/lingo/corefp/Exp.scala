package lingo.corefp

sealed trait Exp {
  def internal_toValue: Value = todo()

  def eval(env: ValueHashMap.Type): Value = todo()
}

sealed trait ExpT[T <: Exp] extends CachedValueT[T] {
}

object ValueExp extends ExpT[Exp] {
  val ValueListExp = ValueListT(ValueExp)
  override val helper = Helper()

  override def internal_apply(x: Exp): Value = x.internal_toValue

  override def internal_unapply(x: Value): Option[Exp] = x match {
    case ExpQuote(v) => Some(v)
    case _ => None
  }
}

final case class Quote(x: Value) extends Exp {
  override def internal_toValue: Value = todo()
}

object ExpQuote extends ExpT[Quote] {
  override val helper = Helper()

  override def internal_apply(x: Quote): Value = x.internal_toValue

  override def internal_unapply(x: Value): Option[Quote] = todo()
}

final case class Commented(comment: Value, x: Exp) extends Exp

// todo
sealed trait Location

final case class UNIXFileLocation(file: String, location: Int) extends Location

final case class Located(location: Location, x: Exp) extends Exp {
  override def eval(env: ValueHashMap.Type): Value = x.eval(env)
}

final case class ApplyFunction(f: Exp, xs: List[Exp]) extends Exp

final case class ApplyMacro(m: Exp, xs: List[Exp]) extends Exp

final case class Var(id: Value) extends Exp {
  override def eval(env: ValueHashMap.Type): Value = env.getOrElse(id, {
    todo()
  })
}

object ExpVar extends ExpT[Var] {
  override val helper = Helper()

  override def internal_apply(x: Var): Value = todo()

  override def internal_unapply(x: Value): Option[Var] = todo()
}

final case class Args(arg: List[Var], rest: Option[Var])

object ValueArgs extends CachedValueT[Args] {
  override val helper: ValueArgs.Helper = Helper()

  override def internal_apply(x: Args): Value = todo()

  override def internal_unapply(x: Value): Option[Args] = todo()
}

final case class Function(arg: Args, body: Exp) extends Exp

final case class Recursive(self: Var, body: Exp) extends Exp {
  override def eval(env: ValueHashMap.Type): Value = {
    lazy val result: PossiblyRecursive = new PossiblyRecursive({
      body.eval(env.updated(self.id, result))
    })
    result
  }
}

final case class GeneralBuiltin(name: Atom, xs: List[Exp])

sealed trait BuiltinFunction extends Exp {
  val name: Atom = this.toGeneral.name

  def toGeneral: GeneralBuiltin = GeneralBuiltin(name, this.getArgsExps)

  def getArgsExps: List[Exp] = this.toGeneral.xs

  protected final def exception(env: ValueHashMap.Type, reason: Atom): Value =
    ExceptionSeq(Atoms.Builtin, reason, ValueHashMap(env), this.name, ValueExp.ValueListExp(this.getArgsExps))

  //protected final def getArgsValues(env: ValueHashMap.Type): List[Value] = this.getArgsExps.map(_.eval(env))
}

abstract class BuiltinFunctionUnary(x: Exp) extends BuiltinFunction {
  final override def toGeneral: GeneralBuiltin = GeneralBuiltin(name, List(x))
}

abstract class BuiltinFunctionBinary(x: Exp, y: Exp) extends BuiltinFunction {
  final override def toGeneral: GeneralBuiltin = GeneralBuiltin(name, List(x, y))
}

abstract class BuiltinFunctionTriple(x: Exp, y: Exp, z: Exp) extends BuiltinFunction {
  final override def toGeneral: GeneralBuiltin = GeneralBuiltin(name, List(x, y, z))
}

final case class IsAtom(x: Exp) extends BuiltinFunctionUnary(x) {
  override val name = Atoms.Builtins.IsAtom

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case Atom(_) => ValueBoolean.True
    case _ => ValueBoolean.False
  }
}

final case class IsEmptyList(x: Exp) extends BuiltinFunctionUnary(x) {
  override val name = Atoms.Builtins.IsEmptyList

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case EmptyList() => ValueBoolean.True
    case _ => ValueBoolean.False
  }
}

final case class IsNonEmptyList(x: Exp) extends BuiltinFunctionUnary(x) {
  override val name = Atoms.Builtins.IsNonEmptyList

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case NonEmptyList(_, _) => ValueBoolean.True
    case _ => ValueBoolean.False
  }
}

final case class IsTagged(x: Exp) extends BuiltinFunctionUnary(x) {
  override val name = Atoms.Builtins.IsTagged

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case Tagged(_, _) => ValueBoolean.True
    case _ => ValueBoolean.False
  }
}

final case class IsException(x: Exp) extends BuiltinFunctionUnary(x) {
  override val name = Atoms.Builtins.IsException

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case Exception(_, _) => ValueBoolean.True
    case _ => ValueBoolean.False
  }
}

final case class IsResource(x: Exp) extends BuiltinFunctionUnary(x) {
  override val name = Atoms.Builtins.IsResource

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case Resource(_, _, _, _) => ValueBoolean.True
    case _ => ValueBoolean.False
  }
}

final case class IntroNonEmptyList(x: Exp, y: Exp) extends BuiltinFunctionBinary(x, y) {
  override val name = Atoms.Builtins.IntroNonEmptyList

  override def eval(env: ValueHashMap.Type): Value = NonEmptyList(x.eval(env), y.eval(env))
}

final case class ElimNonEmptyListHead(x: Exp) extends BuiltinFunctionUnary(x) {
  override val name = Atoms.Builtins.ElimNonEmptyListHead

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case NonEmptyList(a, _) => a
    case _ => this.exception(env, Atoms.ExceptionReasons.TypeMismatch)
  }
}

final case class ElimNonEmptyListTail(x: Exp) extends BuiltinFunctionUnary(x) {
  override val name = Atoms.Builtins.ElimNonEmptyListTail

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case NonEmptyList(_, b) => b
    case _ => this.exception(env, Atoms.ExceptionReasons.TypeMismatch)
  }
}

final case class IntroTagged(x: Exp, y: Exp) extends BuiltinFunctionBinary(x, y) {
  override val name = Atoms.Builtins.IntroTagged

  override def eval(env: ValueHashMap.Type): Value = Tagged(x.eval(env), y.eval(env))
}

final case class ElimTaggedTag(x: Exp) extends BuiltinFunctionUnary(x) {
  override val name = Atoms.Builtins.ElimTaggedTag

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case Tagged(a, _) => a
    case _ => this.exception(env, Atoms.ExceptionReasons.TypeMismatch)
  }
}

final case class ElimTaggedData(x: Exp) extends BuiltinFunctionUnary(x) {
  override val name = Atoms.Builtins.ElimTaggedData

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case Tagged(_, b) => b
    case _ => this.exception(env, Atoms.ExceptionReasons.TypeMismatch)
  }
}

final case class IntroException(x: Exp, y: Exp) extends BuiltinFunctionBinary(x, y) {
  override val name = Atoms.Builtins.IntroException

  override def eval(env: ValueHashMap.Type): Value = Exception(x.eval(env), y.eval(env))
}

final case class ElimExceptionTag(x: Exp) extends BuiltinFunctionUnary(x) {
  override val name = Atoms.Builtins.ElimExceptionTag

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case Exception(a, _) => a
    case _ => this.exception(env, Atoms.ExceptionReasons.TypeMismatch)
  }
}

final case class ElimExceptionData(x: Exp) extends BuiltinFunctionUnary(x) {
  override val name = Atoms.Builtins.ElimExceptionData

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case Exception(_, b) => b
    case _ => this.exception(env, Atoms.ExceptionReasons.TypeMismatch)
  }
}

final case class ElimResourceTag(x: Exp) extends BuiltinFunctionUnary(x) {
  override val name = Atoms.Builtins.ElimResourceTag

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case Resource(a, _, _, _) => a
    case _ => this.exception(env, Atoms.ExceptionReasons.TypeMismatch)
  }

}

final case class ElimResourceData(x: Exp) extends BuiltinFunctionUnary(x) {
  override val name = Atoms.Builtins.ElimResourceData

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case Resource(_, b, _, _) => b
    case _ => this.exception(env, Atoms.ExceptionReasons.TypeMismatch)
  }
}

// Yeah ... ElimBoolean is a function in some models
final case class ElimBoolean(x: Exp, a: Exp, b: Exp) extends BuiltinFunctionTriple(x, a, b) {
  override val name = Atoms.Builtins.ElimBoolean

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case ValueBoolean(x0) => (if (x0) {
      a
    } else {
      b
    }).eval(env)
    case _ => this.exception(env, Atoms.ExceptionReasons.TypeMismatch)
  }
}

final case class Equal(x: Exp, y: Exp) extends BuiltinFunctionBinary(x, y) {
  override val name = Atoms.Builtins.Equal

  override def eval(env: ValueHashMap.Type): Value = ValueBoolean(x.eval(env) equals y.eval(env))
}
