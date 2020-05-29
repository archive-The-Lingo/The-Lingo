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

  private final object NotCached {
    def unapply(x: WHNF): Option[ValueList] = x match {
      case x: ValueList => Some(x)
      case _ => unapplyCore(x.toCore())
    }

    def unapplyCore(x: CoreWHNF): Option[ValueList] = x match {
      case Pair(head, tail) => unapplyCore(tail.reduce_rec_toCore()).map(xs => ValueList(head :: xs.xs))
      case Null() => Some(ValueList(Nil))
      case _ => None
    }
  }

  private val unapply = Value.cached_option_as(NotCached.unapply)

  def unapply(x: Value): Option[ValueList] = unapply.apply(x)
}