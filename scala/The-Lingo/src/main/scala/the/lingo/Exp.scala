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
    CoreException(stack, Symbols.CoreExceptions.NoDefinition, context, this)
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

  private[lingo] override def real_eval(context: Mapping, stack: DebugStack) = TODO()
}

final case class Builtin(f: Sym, xs: List[Value]) extends Exp {
  override def toCore() =
    Exp.consExp(Symbols.Builtin, List(f, ListUtils.ConsList(xs)))

  private[lingo] override def real_eval(context: Mapping, stack: DebugStack) = {
    def evalIs[A](predicate: CoreWHNF => Boolean, x: Value): Value =
      ValueBoolean(predicate(x.eval(context, stack).reduce_rec_toCore()))

    def cons2[A <: WHNF](cons: (Value, Value) => A, x: Value, y: Value): Value =
      cons(x.eval(context, stack), y.eval(context, stack))

    def elim2[A <: WHNF](elim: CoreWHNF => Option[(Value, Value)], exception: Sym, v: Value, idx: Value, idy: Value, exp: Value): Value =
      elim(v.eval(context, stack).reduce_rec_toCore()) match {
        case Some((x, y)) => exp.eval(context.updated(idx, x).updated(idy, y), stack)
        case None => CoreException(stack, exception, context, this)
      }

    (f, xs) match {
      case (Symbols.Builtins.IsPair, x :: Nil) => evalIs(
        _ match {
          case _: Pair => true
          case _ => false
        }, x)
      case (Symbols.Builtins.ConsPair, head :: tail :: Nil) => cons2(Pair, head, tail)
      case (Symbols.Builtins.ElimPair, v :: RemoveComment(Id(idx)) :: RemoveComment(Id(idy)) :: exp :: Nil) =>
        elim2(_ match {
          case Pair(x, y) => Some(x, y)
          case _ => None
        }, Symbols.CoreExceptions.TypeMismatch_Pair, v, idx, idy, exp)

      case (Symbols.Builtins.IsTagged, x :: Nil) => evalIs(
        _ match {
          case _: Tagged => true
          case _ => false
        }, x)
      case (Symbols.Builtins.ConsTagged, tag :: xs :: Nil) => cons2(Tagged, tag, xs)
      case (Symbols.Builtins.ElimTagged, v :: RemoveComment(Id(idx)) :: RemoveComment(Id(idy)) :: exp :: Nil) =>
        elim2(_ match {
          case Tagged(x, y) => Some(x, y)
          case _ => None
        }, Symbols.CoreExceptions.TypeMismatch_Tagged, v, idx, idy, exp)

      case (Symbols.Builtins.IsException, x :: Nil) => evalIs(
        _ match {
          case _: ValueException => true
          case _ => false
        }, x)
      case (Symbols.Builtins.ConsException, tag :: xs :: Nil) => cons2(ValueException, tag, xs)
      case (Symbols.Builtins.ElimException, v :: RemoveComment(Id(idx)) :: RemoveComment(Id(idy)) :: exp :: Nil) =>
        elim2(_ match {
          case ValueException(x, y) => Some(x, y)
          case _ => None
        }, Symbols.CoreExceptions.TypeMismatch_Exception, v, idx, idy, exp)

      case (Symbols.Builtins.Rec, RemoveComment(Id(id)) :: exp :: Nil) => {
        lazy val (innerContext: Mapping, result: Value) = (context.updated(id, result), exp.eval_callByName(innerContext, stack))
        result
      }
      case (Symbols.Builtins.NatToBinary, x :: Nil) => x.eval(context, stack) match {
        case AsCoreWHNF(ValueNat(x)) => ValueList(NatUtils.nat2booleanList(x).map(ValueBoolean(_)))
        case _ => TODO()
      }

      // TODO
      case _ => CoreException(stack, Symbols.CoreExceptions.IllegalExp, context, this)
    }
  }
}
