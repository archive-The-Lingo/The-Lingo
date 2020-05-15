/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo.lang

sealed trait CoreWeakHeadNormalForm extends WeakHeadNormalForm {
  def toCore() = this

  def eval(context: Mapping, stack: DebugStack) = throw new UnsupportedOperationException("TODO")

  def app(xs: List[Value], stack: DebugStack) = throw new UnsupportedOperationException("TODO")

  def equal_reduce_rec(x: Value) = this.equal_core(x.reduce_rec().toCore())

  private[lang] def equal_core(x: CoreWeakHeadNormalForm): Boolean
}

private[lang] final object CoreWeakHead {
  def unapply(x: WeakHeadNormalForm): Option[CoreWeakHeadNormalForm] = Some(x.toCore())
}

final case class Null() extends CoreWeakHeadNormalForm {
  private[lang] def equal_core(x: CoreWeakHeadNormalForm) = x match {
    case Null() => true
    case _ => false
  }
}

final case class Sym(x: Symbol) extends CoreWeakHeadNormalForm {
  private[lang] def equal_core(y: CoreWeakHeadNormalForm) = y match {
    case Sym(y) => x == y
    case _ => false
  }
}

final object Sym {
  def apply(x: Symbol): Sym = Sym(x)

  def apply(x: String): Sym = Sym(Symbol(x))
}

final case class Pair(x: Value, y: Value) extends CoreWeakHeadNormalForm {
  private[lang] def equal_core(ab: CoreWeakHeadNormalForm) = ab match {
    case Pair(a, b) => a.equal_reduce_rec(x) && b.equal_reduce_rec(y)
    case _ => false
  }
}

final case class Tagged(tag: Value, xs: Value) extends CoreWeakHeadNormalForm {
  private[lang] def equal_core(ab: CoreWeakHeadNormalForm) = ab match {
    case Tagged(a, b) => a.equal_reduce_rec(tag) && b.equal_reduce_rec(xs)
    case _ => false
  }
}
