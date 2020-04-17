/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo.lang

final object Value {
  implicit def packValue(x:NotWeakHeadNormalForm):Value = Value(x)
  implicit def packValueList(xs:List[NotWeakHeadNormalForm]):List[Value] = xs.map {Value(_)}
}

final case class Value(var x: NotWeakHeadNormalForm) extends NotWeakHeadNormalForm {
  def reduce_rec() = {
    val result = x.reduce_rec()
    x = result // `x` and `result` are equivalent, so this is thread safe as long as writing is atomic.
    result
  }

  def eval(context: Mapping, stack: DebugStack) = x.eval(context, stack)

  def readback() = x.readback()

  def apply(xs: List[Value], stack: DebugStack) = x.apply(xs, stack)
}

trait NotWeakHeadNormalForm {
  def reduce_rec(): WeakHeadNormalForm

  def eval(context: Mapping, stack: DebugStack): Value

  def readback(): (Mapping, Exp)

  def apply(xs: List[Value], stack: DebugStack): Value
}

trait WeakHeadNormalForm extends NotWeakHeadNormalForm {
  def reduce_rec(): WeakHeadNormalForm = this

  def readback() = (Mapping.Null, Quote(Value(this)))

  def toCore(): CoreWeakHeadNormalForm
}
