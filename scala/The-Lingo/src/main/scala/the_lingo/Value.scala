/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo

final case class Value(var x: NotWeakHeadNormalForm) extends NotWeakHeadNormalForm {
  def reduce() = {
    val result = x.reduce()
    x = result
    result
  }

  def eval(context: Mapping) = x.eval(context)

  def readback() = x.readback()

  def apply(xs: List[Value]) = x.apply(xs)
}

trait NotWeakHeadNormalForm {
  def reduce(): WeakHeadNormalForm

  def eval(context: Mapping): Value

  def readback(): (Mapping, Exp)

  def apply(xs: List[Value]): Value
}

trait WeakHeadNormalForm extends NotWeakHeadNormalForm {
  def reduce(): WeakHeadNormalForm = this

  def readback() = (new Mapping(), Quote(Value(this)))

  def toCore(): CoreWeakHeadNormalForm
}