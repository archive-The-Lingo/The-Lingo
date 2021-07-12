package lingo.corefp

final case class Closure(env: ValueHashMap.Type, args: Args, body: Exp)

object ValueClosure extends CachedValueT[Closure] {
  override val helper = Helper()

  override def internal_apply(x: Closure): Value = TaggedSeq(Atoms.Func, ValueHashMap(x.env), ValueArgs(x.args), ValueExp(x.body))

  override def internal_unapply(x: Value): Option[Closure] = x match {
    case TaggedSeq(Atoms.Func, ValueHashMap(env), ValueArgs(args), ValueExp(body)) => Some(Closure(env, args, body))
    case _ => None
  }
}
