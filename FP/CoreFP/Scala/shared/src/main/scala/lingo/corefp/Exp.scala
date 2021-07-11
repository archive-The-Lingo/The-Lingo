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

sealed trait Builtin extends Exp {

}

sealed trait BuiltinSyntax extends Builtin {

}
sealed trait BuiltinFunction extends Builtin {

}