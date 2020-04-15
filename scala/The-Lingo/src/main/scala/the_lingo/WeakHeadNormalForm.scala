/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo

sealed trait WeakHeadNormalForm extends UnboxedValue {
  def reduce() = this

  def readback(): (Mapping, Exp) = (new Mapping(), Quote(Value(this)))

  def eval(context: Mapping) = throw new UnsupportedOperationException("TODO")

  def apply(xs: List[Value]) = throw new UnsupportedOperationException("TODO")
}

final case class Null() extends WeakHeadNormalForm

final case class Symbol(x: String) extends WeakHeadNormalForm

final case class Pair(x: Value, y: Value) extends WeakHeadNormalForm

final case class Tagged(tag: Value, xs: Value) extends WeakHeadNormalForm