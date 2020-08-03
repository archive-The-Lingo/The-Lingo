/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import the.lingo.Value.Implicits._

final case class ValueString(x: String) extends WHNF {
  override def show(implicit show: MayNotWHNF => String): String = s"ValueString(${"\""}$x${"\""})"

  override def impl_toCore() = Tagged(Symbols.Tags.String, ListUtils.List(ValueList(x.toList.map(ValueChar.apply))))
}

private final object AsValueStringCached {
  private val unapply_v = Value.cached_option_as((arg: WHNF) => arg match {
    case x: ValueString => Some(x)
    case _ => arg.toCore() match {
      case Tagged(AsSym(Symbols.Tags.Char), ListUtils.ConsList(List(AsCharListCached(xs)))) => Some(ValueString(xs.mkString))
      case _ => None
    }
  })

  def unapply(x: Value): Option[ValueString] = unapply_v.apply(x)
}

private final object AsCharListCached {

  import ListHelpers._

  private final object AsValueCharCachedForList {
    def unapply(xs: List[Value]): Option[List[ValueChar]] = xs.flatMapOption(AsValueCharCached.unapply(_))
  }

  def unapply(xs: Value): Option[List[Char]] = xs match {
    case ListUtils.ConsList(AsValueCharCachedForList(xs)) => Some(xs.map(_.x))
    case _ => None
  }

}