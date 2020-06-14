/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

final case class ValueList(xs: List[Value]) extends WHNF {
  override def toCore() = xs match {
    case head :: tail => Pair(head, ValueList(tail))
    case Nil => Null()
  }
}

private final object AsListValueCached {
  private val unapply_v = Value.cached_option_as((arg: WHNF) => arg match {
    case x: ValueList => Some(x)
    case _ => arg.toCore() match {
      case Pair(head, AsListValueCached(tail)) => Some(ValueList(head :: tail.xs))
      case Null() => Some(ValueList(Nil))
      case _ => None
    }
  })

  def unapply(x: Value): Option[ValueList] = unapply_v.apply(x)
}
