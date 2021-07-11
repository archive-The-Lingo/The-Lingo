package lingo.corefp

sealed trait Exp {
  def internal_toValue: Value = todo()

  def eval(env: ValueHashMap.Type): Value = todo()
}

sealed trait ExpT[T <: Exp] extends CachedValueT[T] {
}

object ExpExp extends ExpT[Exp] {
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

final case class Located(location: Location, x: Exp) extends Exp

final case class ApplyFunction(f: Exp, xs: List[Exp]) extends Exp

final case class ApplyMacro(m: Exp, xs: List[Exp]) extends Exp

final case class Var(id: Value) extends Exp

final case class GeneralBuiltin(name: Atom, xs: List[Exp])

sealed trait Builtin extends Exp {
  val name: Atom = todo()

  def toGeneral: GeneralBuiltin = todo()
}

sealed trait BuiltinSyntax extends Builtin {

}

abstract class BuiltinSyntaxBinary(x: Exp, y: Exp) extends BuiltinSyntax {
  final override def toGeneral: GeneralBuiltin = GeneralBuiltin(name, List(x, y))
}

sealed trait BuiltinFunction extends Builtin {

}

abstract class BuiltinFunctionUnary(x: Exp) extends BuiltinFunction {
  final override def toGeneral: GeneralBuiltin = GeneralBuiltin(name, List(x))
}

abstract class BuiltinFunctionBinary(x: Exp, y: Exp) extends BuiltinFunction {
  final override def toGeneral: GeneralBuiltin = GeneralBuiltin(name, List(x, y))
}

final case class IsAtom(x: Exp) extends BuiltinFunctionUnary(x) {
  override val name = Atoms.Builtins.IsAtom
}

final case class IsEmptyList(x: Exp) extends BuiltinFunctionUnary(x) {
  override val name = Atoms.Builtins.IsEmptyList
}

final case class IsNonEmptyList(x: Exp) extends BuiltinFunctionUnary(x) {
  override val name = Atoms.Builtins.IsNonEmptyList
}

final case class IsTagged(x: Exp) extends BuiltinFunctionUnary(x){
  override val name = Atoms.Builtins.IsTagged
}

final case class IsException(x: Exp) extends BuiltinFunctionUnary(x){
  override val name = Atoms.Builtins.IsException
}

final case class IsResource(x: Exp) extends BuiltinFunctionUnary(x){
  override val name = Atoms.Builtins.IsResource
}

final case class IntroNonEmptyList(x: Exp, y: Exp) extends BuiltinFunctionBinary(x, y){
  override val name = Atoms.Builtins.IntroNonEmptyList
}

final case class ElimNonEmptyListHead(x: Exp) extends BuiltinFunctionUnary(x)

final case class ElimNonEmptyListTail(x: Exp) extends BuiltinFunctionUnary(x)

final case class IntroTagged(x: Exp, y: Exp) extends BuiltinFunctionBinary(x, y)

final case class ElimTaggedTag(x: Exp) extends BuiltinFunctionUnary(x)

final case class ElimTaggedData(x: Exp) extends BuiltinFunctionUnary(x)

final case class IntroException(x: Exp, y: Exp) extends BuiltinFunctionBinary(x, y)

final case class ElimExceptionTag(x: Exp) extends BuiltinFunctionUnary(x)

final case class ElimExceptionData(x: Exp) extends BuiltinFunctionUnary(x)

final case class ElimResourceTag(x: Exp) extends BuiltinFunctionUnary(x)

final case class ElimResourceData(x: Exp) extends BuiltinFunctionUnary(x)

final case class ElimBoolean(x: Exp) extends BuiltinFunctionUnary(x)

final case class Equal(x: Exp, y: Exp) extends BuiltinFunctionBinary(x, y)

final case class Function(arg: List[Var], rest: Option[Var], body: Exp) extends Exp

final case class Recursive(self: Var, body: Exp) extends BuiltinSyntaxBinary(self,body)