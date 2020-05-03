/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo.lang

import the_lingo.lang.private_utils.Nat

import scala.util.parsing.input.Position

final case class DebugStack(xs: List[DebugStackElement]) {
  def push(x: DebugStackElement): DebugStack = DebugStack(x :: xs)
}

final case class DebugStackElement(file: String, start: DebugStackElementPos, end: DebugStackElementPos)

final case class DebugStackElementPos(line: Nat, column: Nat)

final object DebugStackElementPos {
  implicit def position2DebugStackElementPos(x: Position): DebugStackElementPos =
    DebugStackElementPos(line = Nat(x.line), column = Nat(x.column))
}