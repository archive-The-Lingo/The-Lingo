/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import the.lingo.utils.Nat

import scala.util.parsing.input.Position

final case class DebugStack(xs: List[DebugStackPosition]) extends WHNF {
  def push(x: DebugStackPosition): DebugStack = DebugStack(x :: xs)

  override def toCore() = Tagged(Symbols.Tags.UNIXFilePositionStack, ListUtils.list(ValueList(xs)))
}

sealed trait DebugStackPosition extends WHNF

final case class NamedPosition(name: Value) extends DebugStackPosition {
  override def toCore() = throw new UnsupportedOperationException("TODO")
}

final case class FilePosition(file: String, start: LineColumn, end: LineColumn) extends DebugStackPosition {
  override def toCore() =
    Tagged(
      Symbols.Tags.UNIXFilePosition,
      ListUtils.list(
        ValueString(file),
        ListUtils.list(
          ValueNat(start.line),
          ValueNat(start.column)),
        ListUtils.list(
          ValueNat(end.line),
          ValueNat(end.column))))
}

private final object AsFilePositionCached {

  private final object NotCached {
    def unapply(x: WHNF): Option[FilePosition] = x match {
      case x: FilePosition => Some(x)
      case _ => unapplyCore(x.toCore())
    }

    def unapplyCore(x: CoreWHNF): Option[FilePosition] = throw new UnsupportedOperationException("TODO")
  }

  private val unapply_v = Value.cached_option_as(NotCached.unapply)

  def unapply(x: Value): Option[FilePosition] = unapply_v.apply(x)
}

final case class LineColumn(line: Nat, column: Nat)

private final object LineColumn {
  implicit def position2LineColumn(x: Position): LineColumn =
    LineColumn(line = Nat(x.line), column = Nat(x.column))
}