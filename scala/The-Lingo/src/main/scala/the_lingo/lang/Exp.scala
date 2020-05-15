/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo.lang

final object Exp {
  private[lang] def consExp(tag: Value, xs: List[Value]): CoreWeakHeadNormalForm =
    Tagged(Symbols.Exp, ListUtils.consList(tag, ListUtils.listToValue(xs)))
}

private[lang] final object AsExp {
  def apply(x: CoreWeakHeadNormalForm): Option[Exp] = AsExp.unapply(x)

  def unapply(x: CoreWeakHeadNormalForm): Option[Exp] = x match {
    case Tagged(NotWeakHead(CoreWeakHead(Symbols.Exp)), xs) => throw new UnsupportedOperationException("TODO")
    case _ => None
  }
}

sealed trait Exp extends WeakHeadNormalForm {
  private[lang] def real_eval(context: Mapping, stack: DebugStack): Value

  def eval(context: Mapping, stack: DebugStack) = Value(new Delay({
    this.real_eval(context, stack)
  }, (context, this)))

  def app(xs: List[Value], stack: DebugStack) = throw new UnsupportedOperationException("TODO")

  def equal_reduce_rec(x: Value) = throw new UnsupportedOperationException("TODO")
}

final case class Id(x: Value) extends Exp {
  def toCore() = Exp.consExp(Symbols.Id, List(x))

  private[lang] def real_eval(context: Mapping, stack: DebugStack) = throw new UnsupportedOperationException("TODO")
}

final case class Quote(x: Value) extends Exp {
  def toCore() = Exp.consExp(Symbols.Quote, List(x))

  private[lang] def real_eval(context: Mapping, stack: DebugStack) = x
}

final case class Comment(comment: Value, x: Value) extends Exp {
  def toCore() = Exp.consExp(Symbols.Comment, List(comment, x))

  private[lang] def real_eval(context: Mapping, stack: DebugStack) = x.eval(context, stack)
}

final case class PositionedExp(pos: DebugStackElement, x: Value) extends Exp {
  def toCore() = throw new UnsupportedOperationException("TODO")

  private[lang] def real_eval(context: Mapping, stack: DebugStack) = x.eval(context, stack.push(pos))
}

final case class ApplyFunc(f: Value, xs: List[Value]) extends Exp {
  def toCore() =
    Exp.consExp(Symbols.ApplyFunc, List(f, ListUtils.listToValue(xs)))

  private[lang] def real_eval(context: Mapping, stack: DebugStack) = f.eval(context, stack).app(xs.map((x: Value) => x.eval(context, stack)), stack)
}

final case class ApplyMacro(f: Value, xs: List[Value]) extends Exp {
  def toCore() =
    Exp.consExp(Symbols.ApplyMacro, List(f, ListUtils.listToValue(xs)))

  private[lang] def real_eval(context: Mapping, stack: DebugStack) = throw new UnsupportedOperationException("TODO")
}

final case class Builtin(f: Value, xs: List[Value]) extends Exp {
  def toCore() =
    Exp.consExp(Symbols.Builtin, List(f, ListUtils.listToValue(xs)))

  private[lang] def real_eval(context: Mapping, stack: DebugStack) = throw new UnsupportedOperationException("TODO")
}
