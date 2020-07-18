/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import the.lingo.Value.Implicits._

final case class CoreException(stack: DebugStack, kind: Sym, context: Mapping, exp: Exp) extends WHNF {
  override def toCore() = ValueException(Symbols.Core, ListUtils.List(kind, context, exp, stack))
}