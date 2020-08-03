/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import the.lingo.Showable.Implicits._
import the.lingo.Value.Implicits._
import the.lingo.utils.Nat

import scala.util.parsing.input.Position

final case class DebugStack(xs: List[DebugStackPosition]) extends WHNF {
  def push(x: DebugStackPosition): DebugStack = DebugStack(x :: xs)

  override def impl_toCore() = ValueList(xs).toCore()

  override def show(implicit show: MayNotWHNF => String): String = s"DebugStack(${xs.show})"
}

final object DebugStack {
  val Empty = DebugStack(Nil)
}

sealed trait DebugStackPosition extends WHNF

final case class NamedPosition(name: Value) extends DebugStackPosition {
  override def impl_toCore() = Tagged(Symbols.Tags.NamedPosition, ListUtils.List(name))

  override def show(implicit show: MayNotWHNF => String): String = s"NamedPosition(${show(name)})"
}

final case class FilePosition(file: String, start: LineColumn, end: LineColumn) extends DebugStackPosition {
  override def impl_toCore() =
    Tagged(
      Symbols.Tags.UNIXFilePosition,
      ListUtils.List(
        ValueString(file),
        ListUtils.List(
          ValueNat(start.line),
          ValueNat(start.column)),
        ListUtils.List(
          ValueNat(end.line),
          ValueNat(end.column))))

  override def show(implicit show: MayNotWHNF => String): String = s"FilePosition(${"\""}${file}${"\""},${start.toString},${end.toString})"
}

private final object AsFilePositionCached {
  private val unapply_v = Value.cached_option_as((arg: WHNF) => arg match {
    case x: FilePosition => Some(x)
    case _ => arg.toCore() match {
      case Tagged(
      AsSym(Symbols.Tags.UNIXFilePosition),
      ListUtils.List(
      AsValueStringCached(ValueString(file)),
      ListUtils.List(AsCoreWHNF(ValueNat(startLine)), AsCoreWHNF(ValueNat(startColumn))),
      ListUtils.List(AsCoreWHNF(ValueNat(endLine)), AsCoreWHNF(ValueNat(endColumn))))) =>
        Some(FilePosition(
          file,
          LineColumn(line = startLine, column = startColumn),
          LineColumn(line = endLine, column = endColumn)))
      case _ => None
    }
  })

  def unapply(x: Value): Option[FilePosition] = unapply_v.apply(x)
}

private final object AsNamedPositionCached {
  private val unapply_v = Value.cached_option_as((arg: WHNF) => arg match {
    case x: NamedPosition => Some(x)
    case _ => arg.toCore() match {
      case Tagged(AsSym(Symbols.Tags.NamedPosition), ListUtils.List(name)) => Some(NamedPosition(name))
      case _ => None
    }
  })

  def unapply(x: Value): Option[NamedPosition] = unapply_v.apply(x)
}

private final object AsDebugStackPositionCached {
  def unapply(x: Value): Option[DebugStackPosition] = x match {
    case AsNamedPositionCached(x) => Some(x)
    case AsFilePositionCached(x) => Some(x)
    case _ => None
  }
}

final case class LineColumn(line: Nat, column: Nat)

private final object LineColumn {
  implicit def position2LineColumn(x: Position): LineColumn =
    LineColumn(line = Nat(x.line), column = Nat(x.column))
}