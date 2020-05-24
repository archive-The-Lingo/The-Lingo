/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo.lang

import the_lingo.lang.private_utils.Nat

import scala.util.parsing.input.Position

final case class DebugStack(xs: List[Pos]) {
  def push(x: Pos): DebugStack = DebugStack(x :: xs)
}

final case class Pos(file: String, start: LineColumn, end: LineColumn) extends WHNF {
  override def toCore() = throw new UnsupportedOperationException("TODO")
}

private final object AsPosNotCached {
  def apply(x: WHNF): Option[Pos] = unapply(x)

  def applyCore(x: CoreWHNF): Option[Pos] = unapplyCore(x)

  def unapply(x: WHNF): Option[Pos] = x match {
    case x: Pos => Some(x)
    case _ => unapplyCore(x.toCore())
  }

  def unapplyCore(x: CoreWHNF): Option[Pos] = throw new UnsupportedOperationException("TODO")
}

private final object AsPosCached {
  val apply = Value.cached_option_as(AsPosNotCached.apply)

  def apply(x: Value): Option[Pos] = apply.apply(x)

  val unapply = apply

  def unapply(x: Value): Option[Pos] = unapply.apply(x)
}

final case class LineColumn(line: Nat, column: Nat)

private final object LineColumn {
  implicit def position2LineColumn(x: Position): LineColumn =
    LineColumn(line = Nat(x.line), column = Nat(x.column))
}