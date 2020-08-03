/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import the.lingo.Value.Implicits._
import the.lingo.Showable.Implicits._

final case class ValueList(xs: List[Value]) extends WHNF {
  override def show(implicit show: MayNotWHNF => String): String = s"ListUtils.List(${xs.showXs})"

  override def impl_toCore() = xs match {
    case head :: tail => Pair(head, ValueList(tail))
    case Nil => Null
  }
}

private final object AsListValueCached {
  private val unapply_v = Value.cached_option_as((arg: WHNF) => arg match {
    case x: ValueList => Some(x)
    case _ => arg.toCore() match {
      case Pair(head, AsListValueCached(tail)) => Some(ValueList(head :: tail.xs))
      case Null => Some(ValueList(Nil))
      case _ => None
    }
  })

  def unapply(x: Value): Option[ValueList] = unapply_v.apply(x)
}
