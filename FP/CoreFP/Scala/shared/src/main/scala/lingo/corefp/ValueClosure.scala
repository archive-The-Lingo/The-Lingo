package lingo.corefp

import lingo.corefp.ValueArgs.valueListDotVar

final case class Closure(env: ValueHashMap.Type, args: Args, body: Exp)

object ValueClosure extends CachedValueT[Closure] {
  override val helper = Helper()

  override def internal_apply(x: Closure): Value = TaggedSeq(Atoms.Func, ValueHashMap(x.env), ValueArgs(x.args), ValueExp(x.body))

  override def internal_unapply(x: Value): Option[Closure] = x match {
    case TaggedSeq(Atoms.Func, ValueHashMap(env), ValueArgs(args), ValueExp(body)) => Some(Closure(env, args, body))
    case _ => None
  }
}

final case class Args(arg: List[Var], rest: Option[Var])

object ValueArgs extends CachedValueT[Args] {
  private val valueListDotVar = ValueListDotTU(ValueVar, ValueVar)
  private val valueListVar = ValueListT(ValueVar)
  override val helper: ValueArgs.Helper = Helper()

  override def internal_apply(x: Args): Value = x match {
    case Args(args, None) => valueListVar(args)
    case Args(args, Some(rest)) => valueListDotVar(ListDot(args, rest))
  }

  override def internal_unapply(x: Value): Option[Args] = x match {
    case valueListVar(args) => Some(Args(args, None))
    case valueListDotVar(ListDot(args, rest)) => Some(Args(args, Some(rest)))
    case _ => None
  }
}
