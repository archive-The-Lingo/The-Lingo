/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import the.lingo.utils.Thunk

// TODO: handle SelfReferenceThunk
final class Delay(continue: => Value, stop: => Exp) extends MayNotWHNF {
  private val valueThunk = new Thunk({
    continue
  })
  private val readbackThunk = new Thunk({
    stop
  })

  override def reduce_rec() = valueThunk().reduce_rec()

  override def reduce() = valueThunk().reduce()

  override def readback() = readbackThunk()

  override def impl_show(implicit showContext: ShowContext): String = valueThunk.synchronized {
    valueThunk.snapshotState match {
      case Thunk.StateDone => valueThunk().show(showContext)
      case Thunk.StateEvaluating | Thunk.StateNone => "Delay(...)"
    }
  }
}

final object Delay {
  def apply(continue: => Value, stop: => Exp): Value = Value(new Delay({
    continue
  }, {
    stop
  }))
}