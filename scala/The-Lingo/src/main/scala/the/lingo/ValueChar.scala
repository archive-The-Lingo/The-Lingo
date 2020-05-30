/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import the.lingo.private_utils.Nat

final case class ValueChar(x: Char) extends WHNF {
  override def toCore() = Tagged(Symbols.Nat, ValueList(List(ValueNat(Nat(x.toInt)))))
}
