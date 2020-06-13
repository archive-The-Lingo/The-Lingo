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
  val True = ValueBoolean(true)
  val False = ValueBoolean(false)
}

private final object AsValueBooleanCached {

  private final object NotCached {
    def unapply(x: WHNF): Option[ValueBoolean] = x match {
      case x: ValueBoolean => Some(x)
      case _ => unapplyCore(x.toCore())
    }

    def unapplyCore(x: CoreWHNF): Option[ValueBoolean] = x match {
      case Tagged(AsSym(Symbols.Tags.True), AsCoreWHNF(Null())) => Some(ValueBoolean.True)
      case Tagged(AsSym(Symbols.Tags.False), AsCoreWHNF(Null())) => Some(ValueBoolean.False)
      case _ => None
    }
  }

  private val unapply_v = Value.cached_option_as(NotCached.unapply)

  def unapply(x: Value): Option[ValueBoolean] = unapply_v.apply(x)
}
