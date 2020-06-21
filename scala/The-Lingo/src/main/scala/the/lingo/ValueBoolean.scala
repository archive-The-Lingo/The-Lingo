/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

final case class ValueBoolean(x: Boolean) extends WHNF {
  override def toCore() = if (x) {
    Tagged(Symbols.Tags.True, Null())
  } else {
    Tagged(Symbols.Tags.False, Null())
  }
}

final object ValueBoolean {
  val True = new ValueBoolean(true)
  val False = new ValueBoolean(false)

  def apply(x: Boolean) = if (x) {
    True
  } else {
    False
  }
}

private final object AsValueBooleanCached {
  private val unapply = Value.cached_option_as((arg: WHNF) => arg match {
    case x: ValueBoolean => Some(x)
    case _ => arg.toCore() match {
      case Tagged(AsSym(Symbols.Tags.True), AsCoreWHNF(Null())) => Some(ValueBoolean.True)
      case Tagged(AsSym(Symbols.Tags.False), AsCoreWHNF(Null())) => Some(ValueBoolean.False)
      case _ => None
    }
  })

  def unapply(x: Value): Option[ValueBoolean] = unapply.apply(x)
}
