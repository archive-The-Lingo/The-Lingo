/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

final object Exp {
  private[lingo] def consExp(tag: Value, xs: List[Value]): CoreWHNF =
    Tagged(Symbols.Exp, ListUtils.list(tag, ListUtils.ConsList(xs)))
}

private final object AsExpCached {

  private final object NotCached {
    def unapply(x: WHNF): Option[Exp] = x match {
      case x: Exp => Some(x)
      case _ => unapplyCore(x.toCore())
    }

    def unapplyCore(x: CoreWHNF): Option[Exp] = x match {
      case Tagged(AsSym(Symbols.Exp), AsCoreWHNF(Pair(AsSym(tag), ListUtils.ConsList(xs)))) =>
        (tag, xs) match {
          case (Symbols.Id, List(x)) => Some(Id(x))
          case (Symbols.Quote, List(x)) => Some(Quote(x))
          case (Symbols.Comment, List(comment, x)) => Some(Comment(comment, x))
          case (Symbols.Positioned, List(AsFilePositionCached(pos), x)) => Some(Positioned(pos, x))
          case (Symbols.ApplyFunc, List(f, ListUtils.ConsList(xs))) => Some(ApplyFunc(f, xs))
          case (Symbols.ApplyMacro, List(f, ListUtils.ConsList(xs))) => Some(ApplyFunc(f, xs))
          case (Symbols.Builtin, List(AsSym(f), ListUtils.ConsList(xs))) => Some(Builtin(f, xs))
          case _ => None
        }
      case _ => None
    }
  }

  private val unapply_v = Value.cached_option_as(NotCached.unapply)

  def unapply(x: Value): Option[Exp] = unapply_v.apply(x)
}

sealed trait Exp extends FeaturedWHN_eval {
  private[lingo] def real_eval(context: Mapping, stack: DebugStack): Value

  final override def feature_eval(context: Mapping, stack: DebugStack) = this.real_eval(context, stack)
}

final case class Id(x: Value) extends Exp {
  override def toCore() = Exp.consExp(Symbols.Id, List(x))

  private[lingo] override def real_eval(context: Mapping, stack: DebugStack) = context.get(x).getOrElse {
    CoreException(stack, Symbols.Exceptions.NoDefinition, context, this)
  }
}

final case class Quote(x: Value) extends Exp {
  override def toCore() = Exp.consExp(Symbols.Quote, List(x))

  private[lingo] override def real_eval(context: Mapping, stack: DebugStack) = x
}

final case class Comment(comment: Value, x: Value) extends Exp {
  override def toCore() = Exp.consExp(Symbols.Comment, List(comment, x))

  private[lingo] override def real_eval(context: Mapping, stack: DebugStack) = x.eval(context, stack)
}

final private object RemoveComment {
  def unapply(x: Value): Option[Exp] = x match {
    case AsExpCached(e) => e match {
      case Comment(_, x) => unapply(x)
      case x => Some(x)
    }
    case _ => None
  }
}

final case class Positioned(pos: DebugStackPosition, x: Value) extends Exp {
  override def toCore() =
    Exp.consExp(Symbols.Positioned, List(pos, x))

  private[lingo] override def real_eval(context: Mapping, stack: DebugStack) = x.eval(context, stack.push(pos))
}

final case class ApplyFunc(f: Value, xs: List[Value]) extends Exp {
  override def toCore() =
    Exp.consExp(Symbols.ApplyFunc, List(f, ListUtils.ConsList(xs)))

  private[lingo] override def real_eval(context: Mapping, stack: DebugStack) = f.eval(context, stack).app(xs.map((x: Value) => x.eval(context, stack)), stack)
}

final case class ApplyMacro(f: Value, xs: List[Value]) extends Exp {
  override def toCore() =
    Exp.consExp(Symbols.ApplyMacro, List(f, ListUtils.ConsList(xs)))

  private[lingo] override def real_eval(context: Mapping, stack: DebugStack) = throw new UnsupportedOperationException("TODO")
}

private final object Builtin {
  def evalIs[A](predicate: CoreWHNF => Boolean, x: Value, context: Mapping, stack: DebugStack): Value =
    ValueBoolean(predicate(x.eval(context, stack).reduce_rec_toCore()))
}

final case class Builtin(f: Sym, xs: List[Value]) extends Exp {
  override def toCore() =
    Exp.consExp(Symbols.Builtin, List(f, ListUtils.ConsList(xs)))

  private[lingo] override def real_eval(context: Mapping, stack: DebugStack) = (f, xs) match {
    case (Symbols.Builtins.IsPair, x :: Nil) => Builtin.evalIs(
      _ match {
        case _: Pair => true
        case _ => false
      }, x, context, stack)
    case (Symbols.Builtins.ConsPair, head :: tail :: Nil) => Pair(head.eval(context, stack), tail.eval(context, stack))

    case (Symbols.Builtins.IsTagged, x :: Nil) => Builtin.evalIs(
      _ match {
        case _: Tagged => true
        case _ => false
      }, x, context, stack)
    case (Symbols.Builtins.ConsTagged, tag :: xs :: Nil) => Tagged(tag.eval(context, stack), xs.eval(context, stack))

    case (Symbols.Builtins.IsException, x :: Nil) => Builtin.evalIs(
      _ match {
        case _: ValueException => true
        case _ => false
      }, x, context, stack)
    case (Symbols.Builtins.ConsException, tag :: xs :: Nil) => ValueException(tag.eval(context, stack), xs.eval(context, stack))

    case (Symbols.Builtins.Rec, RemoveComment(Id(id)) :: exp :: Nil) => {
      lazy val (innerContext: Mapping, result: Value) = (context.updated(id, result), exp.eval_callByName(innerContext, stack))
      result
    }
    // TODO
    case _ => throw new UnsupportedOperationException("TODO")
  }
}
