/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo.lang

import scala.collection.mutable

private final object Value {
  implicit def packValue(x: MayNotWHNF): Value = Value(x)

  implicit def packValueList(xs: List[MayNotWHNF]): List[Value] = xs.map {
    Value(_)
  }

  def cached_option_as[A <: WHNF](f: WHNF => Option[A]): Value => Option[A] =
    (x: Value) => {
      val ptr = x.unpack_rec_to_single_pack()
      val result = f(ptr.reduce_rec())
      result match {
        case Some(v) => {
          ptr.smart_maybe_write(v)
        }
        case None => {}
      }
      result
    }
}

final case class Value(var x: MayNotWHNF) extends MayNotWHNF {
  // writing requires synchronized. reading doesn't
  private def notsynced_maybe_write(v: MayNotWHNF) = {
    x match {
      case _: OpaqueWHNF => {}
      case _ => {
        x = v
      }
    }
  }

  private def smart_maybe_write(v: MayNotWHNF) = {
    val ptr = this.unpack_rec_to_single_pack()
    ptr.synchronized {
      ptr.notsynced_maybe_write(v)
    }
  }

  override def reduce_rec() = {
    val ptr = this.unpack_rec_to_single_pack()
    val result = ptr.reduce_rec()
    ptr.smart_maybe_write(result)
    result
  }

  override def reduce() = {
    val ptr = this.unpack_rec_to_single_pack()
    val result = ptr.reduce()
    assert(result.unpack_rec() ne ptr.unpack_rec())
    ptr.smart_maybe_write(result)
    result
  }

  override def eval(context: Mapping, stack: DebugStack) = x.eval(context, stack)

  override def readback() = x.readback()

  override def app(xs: List[Value], stack: DebugStack) = x.app(xs, stack)

  /*private[lang] def do_unpack_rec(): MayNotWHNF = {
    val history: mutable.HashSet[Value] = new mutable.HashSet() // TODO: check the equality HashSet using (should be ref)
    this.synchronized {
      while (true) {
        x match {
          case v: Value => {
            this.notsynced_maybe_write(v.x)
            if (!history.add(v)) {
              throw new UnsupportedOperationException("TODO: loop")
            }
          }
          case _ => return x
        }
      }
    }
    throw new Exception()
  }*/

  private[lang] def do_unpack_rec(): MayNotWHNF = {
    var iter = this
    // for multi-threading
    while (true) {
      iter.x match {
        case _: Value => {
          iter = iter.unpack_rec_to_single_pack()
        }
        case result => {
          return result
        }
      }
    }
    throw new Exception()
  }

  private def unpack_rec_to_single_pack(): Value = {
    val history: mutable.HashSet[Value] = new mutable.HashSet() // TODO: check the equality HashSet using (should be ref)
    this.synchronized {
      while (true) {
        x match {
          case v@Value(vx: Value) => {
            this.notsynced_maybe_write(vx)
            if (!history.add(v)) {
              throw new UnsupportedOperationException("TODO: loop")
            }
          }
          case x: Value => return x
          case _ => return this
        }
      }
    }
    throw new Exception()
  }
}

trait MayNotWHNF {
  def reduce_rec(): WHNF

  def reduce(): MayNotWHNF = this.reduce_rec()

  def eval(context: Mapping, stack: DebugStack): Value = this.reduce_rec().toCore().eval(context, stack)

  def readback(): Exp

  def app(xs: List[Value], stack: DebugStack): Value = this.reduce_rec().toCore().app(xs, stack)

  def equal_reduce_rec(arg: Value): Boolean = {
    val (self, x) = (this.unpack_rec(), arg.unpack_rec())
    if (self eq x) {
      return true
    }
    val (v0, v1) = (self.reduce_rec(), x.reduce_rec()) match {
      case (x: WHNF, y) => (y, x)
      case (x, y) => (x, y)
    }

    v0.optional_equal_reduce_rec(v1) orElse {
      v1.optional_equal_reduce_rec(v0)
    } match {
      case Some(result) => result // TODO: cache
      case None => v0.toCore().equal_core(v1.toCore())
    }
  }

  final def unpack_rec(): MayNotWHNF = this match {
    case x: Value => x.do_unpack_rec()
    case _ => this
  }
}

private[lang] final object AsWHNF {
  def unapply(x: MayNotWHNF): Option[WHNF] = Some(x.reduce_rec())
}

trait WHNF extends MayNotWHNF {
  final override def reduce_rec(): WHNF = this

  final override def readback() = Quote(this)

  def toCore(): CoreWHNF

  private[lang] def optional_equal_reduce_rec(x: Value): Option[Boolean] = None
}

// GC-ed IO things
trait OpaqueWHNF extends WHNF {

}