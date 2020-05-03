/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo.lang

sealed trait CoreWeakHeadNormalForm extends WeakHeadNormalForm {
  def toCore() = this

  def eval(context: Mapping, stack: DebugStack) = throw new UnsupportedOperationException("TODO")

  def apply(xs: List[Value], stack: DebugStack) = throw new UnsupportedOperationException("TODO")
}

final case class Null() extends CoreWeakHeadNormalForm

final case class Sym(x: Symbol) extends CoreWeakHeadNormalForm

final object Sym {
  def apply(x: Symbol): Sym = Sym(x)

  def apply(x: String): Sym = Sym(Symbol(x))
}

final case class Pair(x: Value, y: Value) extends CoreWeakHeadNormalForm

final case class Tagged(tag: Value, xs: Value) extends CoreWeakHeadNormalForm
