/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo.lang

private final object Value {
  implicit def packValue(x: NotWeakHeadNormalForm): Value = Value(x)

  implicit def packValueList(xs: List[NotWeakHeadNormalForm]): List[Value] = xs.map {
    Value(_)
  }
}

final case class Value(var x: NotWeakHeadNormalForm) extends NotWeakHeadNormalForm {
  def reduce_rec() = {
    val result = x.reduce_rec()
    x = result // `x` and `result` are equivalent, so this is thread safe as long as writing is atomic.
    result
  }

  def reduce() = {
    val result = x.reduce()
    result match {
      case _: WeakHeadNormalForm => x = result
      case _ => {} // cache NotWeakHeadNormalForm may cause problems. For example: x.reduce() = x
    }
    result
  }

  def eval(context: Mapping, stack: DebugStack) = x.eval(context, stack)

  def readback() = x.readback()

  def app(xs: List[Value], stack: DebugStack) = x.app(xs, stack)

  def equal_reduce_rec(y: Value) = x match {
    case _: CoreWeakHeadNormalForm => y.x.equal_reduce_rec(this) // optimization
    case _ => x.equal_reduce_rec(y)
  }
}

trait NotWeakHeadNormalForm {
  def reduce_rec(): WeakHeadNormalForm

  def reduce(): NotWeakHeadNormalForm

  def eval(context: Mapping, stack: DebugStack): Value

  def readback(): (Mapping, Exp)

  def app(xs: List[Value], stack: DebugStack): Value

  def equal_reduce_rec(x: Value): Boolean
}

private[lang] final object NotWeakHead {
  def unapply(x: NotWeakHeadNormalForm): Option[WeakHeadNormalForm] = Some(x.reduce_rec())
}

trait WeakHeadNormalForm extends NotWeakHeadNormalForm {
  def reduce_rec(): WeakHeadNormalForm = this

  def reduce(): NotWeakHeadNormalForm = this

  def readback() = (Mapping.Null, Quote(Value(this)))

  def toCore(): CoreWeakHeadNormalForm
}
