/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo.utils

final class Thunk[T](calc: => T) {

  import Thunk._

  private var state: State = StateNone
  private val value: Init[T] = new Init()

  //only for debug
  private var banned = false

  def ban(): Unit = {
    banned = true
  }

  def snapshotState: State = state

  def apply(): T = value.synchronized {
    assert(!banned)
    this.synchronized {
      state match {
        case StateEvaluating => {
          throw SelfReferenceThunk(this)
        }
        case StateDone => {
          return value.get.get
        }
        case StateNone => {
          state = StateEvaluating
        }
      }
    }
    val result = calc
    val mustTrue = value.set(result)
    assert(mustTrue)
    this.synchronized {
      state match {
        case StateEvaluating => {
          state = StateDone
        }
        case _ => {
          throw new IllegalStateException()
        }
      }
    }
    result
  }
}

object Thunk {

  sealed trait State

  final object StateNone extends State

  final object StateEvaluating extends State

  final object StateDone extends State

}

final case class SelfReferenceThunk[T](causeObject: Thunk[T],
                                       private val message: String = "Self-reference thunk found",
                                       private val cause: Throwable = None.orNull)
  extends Exception(message, cause)