/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import the.lingo.Showable.Implicits._

sealed trait MonadIO extends WHNF {
  def run(): Value
}

final case class MonadIOReturn(x: Value) extends MonadIO {
  override def impl_toCore() = TODO()

  override def impl_show(implicit showContext: ShowContext): String = s"MonadIOReturn"

  override def run() = x
}

final case class MonadIOBind(x: MonadIO, f: Value) extends MonadIO {
  override def impl_toCore() = TODO()

  override def impl_show(implicit showContext: ShowContext): String = s"MonadIOBind(${x.show},${f.show})"

  override def run() = AsMonadIOCached.unapply(f.app(List(x.run()))).get.run()
}

final case class MonadIOOp(f: Sym, args: List[Value]) extends MonadIO {
  override def impl_toCore() = TODO()

  override def impl_show(implicit showContext: ShowContext): String = s"MonadIOOp(${f.show},${args.show})"

  override def run() = TODO()

}

private final object AsMonadIOCached {
  private val unapply_v = Value.cached_option_as((arg: WHNF) => arg match {
    case x: MonadIO => Some(x)
    case _ => arg.toCore() match {
      case _ => TODO()
    }
  })

  def unapply(x: Value): Option[MonadIO] = unapply_v.apply(x)
}