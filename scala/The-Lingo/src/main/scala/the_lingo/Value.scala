/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo

final case class Value(var x: UnboxedValue) extends UnboxedValue {
  def reduce() = x.reduce()

  def eval(context: Mapping) = x.eval(context)

  def readback() = x.readback()

  def apply(xs: List[Value]) = x.apply(xs)
}

trait UnboxedValue {
  def reduce(): WeakHeadNormalForm

  def eval(context: Mapping): Value

  def readback(): (Mapping, Exp)

  def apply(xs: List[Value]): Value
}