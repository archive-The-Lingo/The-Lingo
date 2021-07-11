package lingo.corefp

sealed trait Exp {
  def internal_toValue: Value = todo()

  def eval(env: ValueHashMap.Type): Value = todo()
}

object ValueExp extends CachedValueT[Exp] {
  override val helper = Helper()

  override def internal_apply(x: Exp): Value = x.internal_toValue

  override def internal_unapply(x: Value): Option[Exp] = x match {
    case ValueQuote(v) => Some(v)
    case _ => None
  }
}

final case class Quote(x: Value) extends Exp {
  override def internal_toValue: Value = todo()
}

object ValueQuote extends CachedValueT[Quote] {
  override val helper = Helper()

  override def internal_apply(x: Quote): Value = x.internal_toValue

  override def internal_unapply(x: Value): Option[Quote] = todo()
}

final case class Commented(comment: Value, x: Exp) extends Exp

// todo
final case class Location()

final case class Located(location: Location, x: Exp) extends Exp

final case class ApplyFunction(f: Exp, xs: List[Exp]) extends Exp

final case class ApplyMacro(m: Exp, xs: List[Exp]) extends Exp

final case class Var(id: Value) extends Exp

sealed trait Builtin extends Exp {

}

sealed trait BuiltinSyntax extends Builtin {

}

sealed trait BuiltinFunction extends Builtin {

}

final case class IsAtom(x: Exp) extends BuiltinFunction

final case class IsEmptyList(x: Exp) extends BuiltinFunction

final case class IsNonEmptyList(x: Exp) extends BuiltinFunction

final case class IsTagged(x: Exp) extends BuiltinFunction

final case class IsException(x: Exp) extends BuiltinFunction

final case class IsResource(x: Exp) extends BuiltinFunction

final case class IntroNonEmptyList(x: Exp, y: Exp) extends BuiltinFunction
final case class ElimNonEmptyListHead(x:Exp) extends BuiltinFunction
final case class ElimNonEmptyListTail(x:Exp) extends BuiltinFunction
final case class IntroTagged(x: Exp, y: Exp) extends BuiltinFunction
final case class ElimTaggedTag(x:Exp) extends BuiltinFunction
final case class ElimTaggedData(x:Exp) extends BuiltinFunction
final case class IntroException(x: Exp, y: Exp) extends BuiltinFunction
final case class ElimExceptionTag(x:Exp) extends BuiltinFunction
final case class ElimExceptionData(x:Exp) extends BuiltinFunction

final case class ElimResourceTag(x:Exp) extends BuiltinFunction
final case class ElimResourceData(x:Exp) extends BuiltinFunction


final case class Function(arg: List[Var], rest: Option[Var], body: Exp) extends Exp

final case class Recursive(self: Var, body: Exp) extends Exp