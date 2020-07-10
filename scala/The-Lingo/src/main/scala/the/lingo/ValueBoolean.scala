/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import the.lingo.Value.Implicits._

sealed trait ValueBoolean extends WHNF {
  def toBoolean: Boolean
}

final object ValueBoolean {

  final case object True extends ValueBoolean {
    override def toCore() = Tagged(Symbols.Tags.True, Null)

    override def toBoolean = true
  }

  final case object False extends ValueBoolean {
    override def toCore() = Tagged(Symbols.Tags.False, Null)

    override def toBoolean = false
  }

  def apply(x: Boolean) = if (x) {
    True
  } else {
    False
  }
}

private final object AsValueBooleanCached {
  private val unapply_v = Value.cached_option_as((arg: WHNF) => arg match {
    case x: ValueBoolean => Some(x)
    case _ => arg.toCore() match {
      case Tagged(AsSym(Symbols.Tags.True), AsCoreWHNF(Null)) => Some(ValueBoolean.True)
      case Tagged(AsSym(Symbols.Tags.False), AsCoreWHNF(Null)) => Some(ValueBoolean.False)
      case _ => None
    }
  })

  def unapply(x: Value): Option[ValueBoolean] = unapply_v.apply(x)
}
