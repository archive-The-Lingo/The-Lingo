/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

final class Delay(continue: => Value, stop: => Exp) extends MayNotWHNF {
  private lazy val cont = {
    counted = true
    continue
  }
  private var counted = false
  private lazy val readbck = stop

  override def reduce_rec() = cont.reduce_rec()

  override def reduce() = cont.reduce()

  override def readback() = readbck

  override def impl_show(implicit showContext: ShowContext): String = if (counted) {
    cont.show(showContext)
  } else {
    s"Delay(...)"
  }
}

final object Delay {
  def apply(continue: => Value, stop: => Exp): Value = Value(new Delay({
    continue
  }, {
    stop
  }))
}