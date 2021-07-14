package lingo.corefp

sealed abstract class Exp(name: Atom, xs: List[Value]) {
  def eval(env: ValueHashMap.Type): Value = todo()

  def toValue: Value = todo()
}

object ValueExp extends CachedValueT[Exp] {
  val ValueListExp = ValueListT(ValueExp)
  override val helper = Helper()

  override def internal_apply(x: Exp): Value = x match {
    case _ => todo()
  }

  override def internal_unapply(x: Value): Option[Exp] = x match {
    case _ => None
  }
}

final case class Quote(x: Value) extends Exp(Atoms.Exps.Quote, List(x)) {
}


final case class Commented(comment: Value, x: Exp) extends Exp(Atoms.Exps.Commented, List(comment, ValueExp(x)))

// todo
sealed trait Location

object ValueLocation extends CachedValueT[Location] {
  override val helper = Helper()

  override def internal_apply(x: Location): Value = todo()

  override def internal_unapply(x: Value): Option[Location] = todo()
}

final case class UNIXFileLocation(file: String, location: Int) extends Location

final case class Located(location: Location, x: Exp) extends Exp(Atoms.Exps.Located, List(ValueLocation(location), ValueExp(x))) {
  override def eval(env: ValueHashMap.Type): Value = x.eval(env)
}


final case class ApplyFunction(f: Exp, xs: List[Exp]) extends Exp(Atoms.Exps.ApplyFunction, List(ValueExp(f), ValueExp.ValueListExp(xs)))

final case class ApplyMacro(m: Exp, xs: List[Exp]) extends Exp(Atoms.Exps.ApplyMacro, List(ValueExp(m), ValueExp.ValueListExp(xs)))

final case class Var(id: Value) extends Exp(Atoms.Exps.Var, List(id)) {
  override def eval(env: ValueHashMap.Type): Value = env.getOrElse(id, {
    todo()
  })
}

object ValueVar extends CachedValueT[Var] {
  override protected val helper = Helper()

  override protected def internal_apply(x: Var): Value = x.toValue

  override protected def internal_unapply(x: Value): Option[Var] = todo()
}

final case class Function(args: Args, body: Exp) extends Exp(Atoms.Func, List(ValueArgs(args), ValueExp(body))) {
  override def eval(env: ValueHashMap.Type): Value = ValueClosure(Closure(env, args, body))
}

final case class Recursive(self: Var, body: Exp) extends Exp(Atoms.Exps.Recursive, List(ValueExp(self), ValueExp(body))) {
  override def eval(env: ValueHashMap.Type): Value = {
    lazy val result: PossiblyRecursive = new PossiblyRecursive({
      body.eval(env.updated(self.id, result))
    })
    result
  }
}

sealed abstract class Builtin(name: Atom, xs: List[Exp]) extends Exp(Atoms.Builtin, List(name, ValueExp.ValueListExp(xs))) {

  protected final def exception(env: ValueHashMap.Type, reason: Atom): Value =
    ExceptionSeq(Atoms.Builtin, reason, ValueHashMap(env), this.name, ValueExp.ValueListExp(this.xs))

}

sealed abstract class BuiltinFunctionUnary(name: Atom, x: Exp) extends Builtin(name, List(x)) {
}

sealed abstract class BuiltinFunctionBinary(name: Atom, x: Exp, y: Exp) extends Builtin(name, List(x, y)) {
}

sealed abstract class BuiltinFunctionTriple(name: Atom, x: Exp, y: Exp, z: Exp) extends Builtin(name, List(x, y, z)) {
}

final case class IsAtom(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.IsAtom, x) {

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case Atom(_) => ValueBoolean.True
    case _ => ValueBoolean.False
  }
}

final case class IsEmptyList(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.IsEmptyList, x) {

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case EmptyList() => ValueBoolean.True
    case _ => ValueBoolean.False
  }
}

final case class IsNonEmptyList(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.IsNonEmptyList, x) {

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case NonEmptyList(_, _) => ValueBoolean.True
    case _ => ValueBoolean.False
  }
}

final case class IsTagged(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.IsTagged, x) {

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case Tagged(_, _) => ValueBoolean.True
    case _ => ValueBoolean.False
  }
}

final case class IsException(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.IsException, x) {

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case Exception(_, _) => ValueBoolean.True
    case _ => ValueBoolean.False
  }
}

final case class IsResource(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.IsResource, x) {

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case Resource(_, _, _, _) => ValueBoolean.True
    case _ => ValueBoolean.False
  }
}

final case class IntroNonEmptyList(x: Exp, y: Exp) extends BuiltinFunctionBinary(Atoms.Builtins.IntroNonEmptyList, x, y) {

  override def eval(env: ValueHashMap.Type): Value = NonEmptyList(x.eval(env), y.eval(env))
}

final case class ElimNonEmptyListHead(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.ElimNonEmptyListHead, x) {

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case NonEmptyList(a, _) => a
    case _ => this.exception(env, Atoms.ExceptionReasons.TypeMismatch)
  }
}

final case class ElimNonEmptyListTail(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.ElimNonEmptyListTail, x) {

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case NonEmptyList(_, b) => b
    case _ => this.exception(env, Atoms.ExceptionReasons.TypeMismatch)
  }
}

final case class IntroTagged(x: Exp, y: Exp) extends BuiltinFunctionBinary(Atoms.Builtins.IntroTagged, x, y) {

  override def eval(env: ValueHashMap.Type): Value = Tagged(x.eval(env), y.eval(env))
}

final case class ElimTaggedTag(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.ElimTaggedTag, x) {

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case Tagged(a, _) => a
    case _ => this.exception(env, Atoms.ExceptionReasons.TypeMismatch)
  }
}

final case class ElimTaggedData(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.ElimTaggedData, x) {

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case Tagged(_, b) => b
    case _ => this.exception(env, Atoms.ExceptionReasons.TypeMismatch)
  }
}

final case class IntroException(x: Exp, y: Exp) extends BuiltinFunctionBinary(Atoms.Builtins.IntroException, x, y) {

  override def eval(env: ValueHashMap.Type): Value = Exception(x.eval(env), y.eval(env))
}

final case class ElimExceptionTag(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.ElimExceptionTag, x) {

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case Exception(a, _) => a
    case _ => this.exception(env, Atoms.ExceptionReasons.TypeMismatch)
  }
}

final case class ElimExceptionData(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.ElimExceptionData, x) {

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case Exception(_, b) => b
    case _ => this.exception(env, Atoms.ExceptionReasons.TypeMismatch)
  }
}

final case class ElimResourceTag(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.ElimResourceTag, x) {

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case Resource(a, _, _, _) => a
    case _ => this.exception(env, Atoms.ExceptionReasons.TypeMismatch)
  }

}

final case class ElimResourceData(x: Exp) extends BuiltinFunctionUnary(Atoms.Builtins.ElimResourceData, x) {

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case Resource(_, b, _, _) => b
    case _ => this.exception(env, Atoms.ExceptionReasons.TypeMismatch)
  }
}

// Yeah ... ElimBoolean is a function in some models
final case class ElimBoolean(x: Exp, a: Exp, b: Exp) extends BuiltinFunctionTriple(Atoms.Builtins.ElimBoolean, x, a, b) {

  override def eval(env: ValueHashMap.Type): Value = x.eval(env) match {
    case ValueBoolean(x0) => (if (x0) {
      a
    } else {
      b
    }).eval(env)
    case _ => this.exception(env, Atoms.ExceptionReasons.TypeMismatch)
  }
}

final case class Equal(x: Exp, y: Exp) extends BuiltinFunctionBinary(Atoms.Builtins.Equal, x, y) {

  override def eval(env: ValueHashMap.Type): Value = ValueBoolean(x.eval(env) equals y.eval(env))
}
