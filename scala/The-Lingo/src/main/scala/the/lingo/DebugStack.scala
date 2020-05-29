/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import the.lingo.private_utils.Nat

import scala.util.parsing.input.Position

final case class DebugStack(xs: List[Pos]) {
  def push(x: Pos): DebugStack = DebugStack(x :: xs)
}

final case class Pos(file: String, start: LineColumn, end: LineColumn) extends WHNF {
  override def toCore() = throw new UnsupportedOperationException("TODO")
}

private final object AsPosCached {

  private final object NotCached {
    def unapply(x: WHNF): Option[Pos] = x match {
      case x: Pos => Some(x)
      case _ => unapplyCore(x.toCore())
    }

    def unapplyCore(x: CoreWHNF): Option[Pos] = throw new UnsupportedOperationException("TODO")
  }

  private val unapply = Value.cached_option_as(NotCached.unapply)

  def unapply(x: Value): Option[Pos] = unapply.apply(x)
}

final case class LineColumn(line: Nat, column: Nat)

private final object LineColumn {
  implicit def position2LineColumn(x: Position): LineColumn =
    LineColumn(line = Nat(x.line), column = Nat(x.column))
}