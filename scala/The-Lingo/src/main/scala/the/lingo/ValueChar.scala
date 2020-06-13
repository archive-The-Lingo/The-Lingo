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
  private val unapply = Value.cached_option_as((arg: WHNF) => arg match {
    case x: ValueChar => Some(x)
    case _ => throw new UnsupportedOperationException("TODO")
  })

  def unapply(x: CoreWHNF): Option[ValueChar] = unapply.apply(x)
}
