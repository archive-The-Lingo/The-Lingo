package lingo.corefp

final case class Closure(env: ValueHashMap.Type, args: Args, body: Exp) {
  def apply(xs: List[Value])(implicit debugStack: MaybeDebugStack): Option[Value] = args.matchArgs(xs, env).map(body.eval(_, implicitly))
  // case None => Builtin.exception(Atoms.ApplyFunction, todo(), ValueHashMap.EmptyMap, Atoms.ExceptionReasons.ArgsMismatch)
}

object ValueClosure extends CachedValueT[Closure] {
  override val helper = Helper()

  override def internal_apply(x: Closure): Value = TaggedSeq(Atoms.Func, ValueHashMap(x.env), ValueArgs(x.args), ValueExp(x.body))

  override def internal_unapply(x: Value): Option[Closure] = x match {
    case TaggedSeq(Atoms.Func, ValueHashMap(env), ValueArgs(args), ValueExp(body)) => Some(Closure(env, args, body))
    case _ => None
  }
}

final case class Args(args: List[Var], rest: Option[Var]) {
  def matchArgs(xs: List[Value]): Option[ValueHashMap.Type] = this.matchArgs(xs, ValueHashMap.EmptyMap)

  def matchArgs(xs: List[Value], env: ValueHashMap.Type): Option[ValueHashMap.Type] = (args, rest, xs) match {
    case (Nil, None, Nil) => Some(env)
    case (Nil, Some(Var(v)), xs) => Some(env.updated(v, ValueList(xs)))
    case (Var(a) :: args, rest, x :: xs) => Args(args, rest).matchArgs(xs, env.updated(a, x))
    case (_ :: _, _, Nil) => None
    case (Nil, None, _) => None
  }
}

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

final case class Macro(x: Closure)

object ValueMacro extends CachedValueT[Macro] {
  override val helper = Helper()

  override def internal_apply(x: Macro): Value = TaggedSeq(Atoms.Tags.Macro, ValueClosure(x.x))

  override def internal_unapply(x: Value): Option[Macro] = x match {
    case TaggedSeq(Atoms.Tags.Macro, ValueClosure(x)) => Some(Macro(x))
    case _ => None
  }
}