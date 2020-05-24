/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo.lang

final object Exp {
  private[lang] def consExp(tag: Value, xs: List[Value]): CoreWHNF =
    Tagged(Symbols.Exp, ListUtils.consList(tag, ListUtils.ValueList(xs)))
}

private final object AsExpNotCached {
  def apply(x: WHNF): Option[Exp] = unapply(x)

  def applyCore(x: CoreWHNF): Option[Exp] = unapplyCore(x)

  def unapply(x: WHNF): Option[Exp] = x match {
    case x: Exp => Some(x)
    case _ => unapplyCore(x.toCore())
  }

  def unapplyCore(x: CoreWHNF): Option[Exp] = x match {
    case Tagged(AsCoreWHNF(Symbols.Exp), AsCoreWHNF(Pair(tag, ListUtils.ValueList(xs)))) =>
      Tuple2(tag.reduce_rec().toCore(), xs) match {
        case (Symbols.Id, List(x)) => Some(Id(x))
        case (Symbols.Quote, List(x)) => Some(Quote(x))
        case (Symbols.Comment, List(comment, x)) => Some(Comment(comment, x))
        case (Symbols.Positioned, List(AsPosCached(pos), x)) => Some(Positioned(pos, x))
        case (Symbols.ApplyFunc, List(f, ListUtils.ValueList(xs))) => Some(ApplyFunc(f, xs))
        case (Symbols.ApplyMacro, List(f, ListUtils.ValueList(xs))) => Some(ApplyFunc(f, xs))
        case (Symbols.Builtin, List(f, ListUtils.ValueList(xs))) => Some(Builtin(f, xs))
        case _ => None
      }
    case _ => None
  }
}

private final object AsExpCached {
  val apply = Value.cached_option_as(AsExpNotCached.apply)

  def apply(x: Value): Option[Exp] = apply.apply(x)

  val unapply = apply

  def unapply(x: Value): Option[Exp] = unapply.apply(x)
}

sealed trait Exp extends WHNF {
  private[lang] def real_eval(context: Mapping, stack: DebugStack): Value

  final override def eval(context: Mapping, stack: DebugStack) = Delay({
    this.real_eval(context, stack)
  }, {
    Builtin(Symbols.Eval, List(Quote(context), Quote(this)))
  })
}

final case class Id(x: Value) extends Exp {
  override def toCore() = Exp.consExp(Symbols.Id, List(x))

  private[lang] override def real_eval(context: Mapping, stack: DebugStack) = throw new UnsupportedOperationException("TODO")
}

final case class Quote(x: Value) extends Exp {
  override def toCore() = Exp.consExp(Symbols.Quote, List(x))

  private[lang] override def real_eval(context: Mapping, stack: DebugStack) = x
}

final case class Comment(comment: Value, x: Value) extends Exp {
  override def toCore() = Exp.consExp(Symbols.Comment, List(comment, x))

  private[lang] override def real_eval(context: Mapping, stack: DebugStack) = x.eval(context, stack)
}

final case class Positioned(pos: Pos, x: Value) extends Exp {
  override def toCore() =
    Exp.consExp(Symbols.Positioned, List(pos, x))

  private[lang] override def real_eval(context: Mapping, stack: DebugStack) = x.eval(context, stack.push(pos))
}

final case class ApplyFunc(f: Value, xs: List[Value]) extends Exp {
  override def toCore() =
    Exp.consExp(Symbols.ApplyFunc, List(f, ListUtils.ValueList(xs)))

  private[lang] override def real_eval(context: Mapping, stack: DebugStack) = f.eval(context, stack).app(xs.map((x: Value) => x.eval(context, stack)), stack)
}

final case class ApplyMacro(f: Value, xs: List[Value]) extends Exp {
  override def toCore() =
    Exp.consExp(Symbols.ApplyMacro, List(f, ListUtils.ValueList(xs)))

  private[lang] override def real_eval(context: Mapping, stack: DebugStack) = throw new UnsupportedOperationException("TODO")
}

final case class Builtin(f: Value, xs: List[Value]) extends Exp {
  override def toCore() =
    Exp.consExp(Symbols.Builtin, List(f, ListUtils.ValueList(xs)))

  private[lang] override def real_eval(context: Mapping, stack: DebugStack) = throw new UnsupportedOperationException("TODO")
}
