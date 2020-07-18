/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import the.lingo.Value.Implicits._
import the.lingo.utils.Nat

final case class ValueChar(x: Char) extends WHNF {
  override def toCore() = Tagged(Symbols.Tags.Char, ListUtils.List(ValueNat(Nat(x.toInt))))
}

private final object AsValueCharCached {
  private val unapply_v = Value.cached_option_as((arg: WHNF) => arg match {
    case x: ValueChar => Some(x)
    case _ => arg.toCore() match {
      case Tagged(AsSym(Symbols.Tags.Char), ListUtils.ConsList(List(AsCoreWHNF(ValueNat(x))))) => Some(ValueChar(x.toChar))
      case _ => None
    }
  })

  def unapply(x: Value): Option[ValueChar] = unapply_v.apply(x)
}
