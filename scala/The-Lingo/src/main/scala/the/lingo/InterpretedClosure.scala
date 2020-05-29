/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

final case class InterpretedClosure(args: List[Value], vararg: Option[Value], context: Mapping, exp: Value) extends WHNF with WHNFFeature_app {
  override def toCore() = throw new UnsupportedOperationException("TODO")

  override def feature_app(xs: List[Value], stack: DebugStack) = InterpretedClosure.match_args(args, vararg, context, xs) match {
    case Some(context) => exp.eval(context, stack)
    case None => throw new UnsupportedOperationException("TODO")
  }
}

private final object InterpretedClosure {
  private[InterpretedClosure] def match_args(args: List[Value], vararg: Option[Value], context: Mapping, xs: List[Value]): Option[Mapping] = (args, vararg, xs) match {
    case (Nil, Some(vararg), xs) => Some(context.updated(vararg, ValueList(xs)))
    case (Nil, None, Nil) => Some(context)
    case (id :: args, vararg, x :: xs) => match_args(args, vararg, context.updated(id, x), xs)
    case _ => None
  }
}

private final object AsInterpretedClosureCached {

  private final object NotCached {
    def unapply(x: WHNF): Option[InterpretedClosure] = x match {
      case x: InterpretedClosure => Some(x)
      case _ => unapplyCore(x.toCore())
    }

    def unapplyCore(x: CoreWHNF): Option[InterpretedClosure] = throw new UnsupportedOperationException("TODO")
  }

  private val unapply = Value.cached_option_as(NotCached.unapply)

  def unapply(x: Value): Option[InterpretedClosure] = unapply.apply(x)
}
