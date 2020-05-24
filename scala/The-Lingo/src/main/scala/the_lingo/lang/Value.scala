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
      val result = f(x.reduce_rec())
      result match {
        case Some(v) => {
          x.synchronized {
            x.notsynced_maybe_write(v)
          }
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

  override def reduce_rec() = {
    val result = x.reduce_rec()
    this.synchronized {
      this.notsynced_maybe_write(result)
    }
    result
  }

  override def reduce() = {
    val result = x.unpack_rec().reduce()
    result match {
      case _: WHNF => this.synchronized {
        this.notsynced_maybe_write(result)
      }
      case _ => {} // cache NotWeakHeadNormalForm may cause problems. For example: x.reduce() = x
    }
    result
  }

  override def eval(context: Mapping, stack: DebugStack) = x.eval(context, stack)

  override def readback() = x.readback()

  override def app(xs: List[Value], stack: DebugStack) = x.app(xs, stack)

  private[lang] def do_unpack_rec(): MayNotWHNF = {
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