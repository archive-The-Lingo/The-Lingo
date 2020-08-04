/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import the.lingo.Value.Implicits._
import the.lingo.Showable.Implicits._

final case class InterpretedClosure(args: List[Id], vararg: Option[Id], context: Mapping, exp: Value) extends FeaturedWHNF_app {
  override def impl_toCore() = Tagged(
    Symbols.Func,
    ListUtils.List(
      vararg match {
        case Some(vararg) => ListUtils.ConsListMaybeWithTail(args, vararg)
        case None => ListUtils.ConsList(args)
      },
      if (context.isEmpty) {
        exp
      } else {
        Builtin(Symbols.Builtins.Eval, List(Quote(context), Quote(exp)))
      }))

  override def feature_app(xs: List[Value], stack: DebugStack) = InterpretedClosure.match_args(args, vararg, context, xs) match {
    case Some(context) => exp.eval(context, stack)
    case None => CoreException(stack, Symbols.CoreExceptions.ArgsMismatch, Mapping.Empty, ApplyFunc(Quote(this), xs.map(Quote(_))))
  }

  override def impl_show(implicit showContext: ShowContext): String = s"InterpretedClosure(${args.show},${vararg.show},${context.show},${exp.show})"
}

private final object InterpretedClosure {
  private[InterpretedClosure] def match_args(args: List[Id], vararg: Option[Id], context: Mapping, xs: List[Value]): Option[Mapping] = (args, vararg, xs) match {
    case (Nil, Some(Id(vararg)), xs) => Some(context.updated(vararg, ValueList(xs)))
    case (Nil, None, Nil) => Some(context)
    case (Id(id) :: args, vararg, x :: xs) => match_args(args, vararg, context.updated(id, x), xs)
    case _ => None
  }
}

private final object AsInterpretedClosureCached {

  private[lingo] final object AsIdList {

    import ListHelpers._

    def unapply(xs: List[Value]): Option[List[Id]] = xs.flatMapOption(
      _ match {
        case RemoveWrapper(x: Id) => Some(x)
        case _ => None
      })
  }

  private val unapply_v = Value.cached_option_as((arg: WHNF) => arg match {
    case x: InterpretedClosure => Some(x)
    case _ => arg.toCore() match {
      case Tagged(
      AsSym(Symbols.Func),
      ListUtils.ConsList(List(ListUtils.ConsList(AsIdList(args)), exp))) =>
        Some(InterpretedClosure(
          args,
          None,
          Mapping.Empty,
          exp))
      case Tagged(
      AsSym(Symbols.Func),
      ListUtils.ConsList(List(ListUtils.ConsListMaybeWithTail(AsIdList(args), RemoveWrapper(tail: Id)), exp))) =>
        Some(InterpretedClosure(
          args,
          Some(tail),
          Mapping.Empty,
          exp))
      case _ => None
    }
  })

  def unapply(x: Value): Option[InterpretedClosure] = unapply_v.apply(x)
}
