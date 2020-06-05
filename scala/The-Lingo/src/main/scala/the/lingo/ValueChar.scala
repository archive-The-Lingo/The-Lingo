/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import the.lingo.utils.Nat

final case class ValueChar(x: Char) extends WHNF {
  override def toCore() = Tagged(Symbols.Nat, ListUtils.list(ValueNat(Nat(x.toInt))))
}

private final object AsValueCharCached {

  private final object NotCached {
    def unapply(x: WHNF): Option[ValueChar] = x match {
      case x: ValueChar => Some(x)
      case _ => unapplyCore(x.toCore())
    }

    def unapplyCore(x: CoreWHNF): Option[ValueChar] = throw new UnsupportedOperationException("TODO")
  }

  private val unapply_v = Value.cached_option_as(NotCached.unapply)

  def unapply(x: Value): Option[ValueChar] = unapply_v.apply(x)
}
