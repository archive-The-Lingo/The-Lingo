/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

sealed trait MonadIO extends WHNF {

}

final case object MonadIOReturn extends MonadIO {
  override def impl_toCore() = TODO()

  override def impl_show(implicit showContext: ShowContext): String = s"MonadIOReturn"
}

final case class MonadIOBind(x: MonadIO, f: InterpretedClosure) extends MonadIO {
  override def impl_toCore() = TODO()

  override def impl_show(implicit showContext: ShowContext): String = s"MonadIOBind(${x.show},${f.show})"
}

sealed trait MonadIOOp extends MonadIO {

}

final object MonadIOOp {
  // TODO

}
