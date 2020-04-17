/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo.lang

import the_lingo.lang.private_utils.Nat

final case class DebugStack(xs: List[DebugStackElement]) {
  // TODO
}

sealed trait DebugStackElement

final case class DebugStackElement_StructuredUnixFile(file: String, addr: List[Nat]) extends DebugStackElement

final case class DebugStackElement_UnixFile(file: String, line: Nat) extends DebugStackElement