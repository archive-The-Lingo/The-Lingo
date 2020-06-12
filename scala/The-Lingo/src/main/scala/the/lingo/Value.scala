/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import scala.collection.mutable

private final object Value {
  implicit def implicitPackWHNF[A <: WHNF](x: A): Value = Value(x)

  implicit def implicitPackWHNFList[A <: WHNF](xs: List[A]): List[Value] = xs.map {
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

final case class Value(private var x: MayNotWHNF) extends MayNotWHNF {
  // writing requires synchronized. reading doesn't
  private def notsynced_unsafe_write(v: MayNotWHNF) = {
    x = v
  }

  private def notsynced_single_pack_maybe_write(v: MayNotWHNF) = {
    x match {
      case _: Value => throw new AssertionError("not single pack")
      case _: OpaqueWHNF => {}
      case _ => {
        x = v
      }
    }
  }

  private def smart_maybe_write(v: MayNotWHNF): Unit = {
    var ptr = this
    // for multi-threading
    while (true) {
      ptr.synchronized {
        ptr.x match {
          case _: Value => {}
          case _ => {
            ptr.notsynced_single_pack_maybe_write(v)
            return
          }
        }
      }
      ptr = ptr.unpack_rec_to_single_pack()
    }
    throw new Exception()
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
    assert(result.unpack_rec() ne ptr.unpack_rec()) // avoid loops. example: result = Value(ptr)
    ptr.smart_maybe_write(result)
    result
  }

  override def readback() = x.readback()

  private[lingo] def do_unpack_rec(): MayNotWHNF = {
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
          case v: Value => {
            // no need "v.synchronized" since it's just reading
            v.x match {
              case vx: Value => {
                this.notsynced_unsafe_write(vx)
                if (!history.add(v)) {
                  throw new UnsupportedOperationException("TODO: loop")
                }
              }
              case _ => return v
            }
          }
          case _ => return this
        }
      }
    }
    throw new Exception()
  }

  def equal_reduce_rec(arg: Value): Boolean = {
    // TODO: cache
    val (self, x) = (this.unpack_rec(), arg.unpack_rec())
    if (self eq x) {
      return true
    }
    val (v0, v1) = (self.reduce_rec(), x.reduce_rec())
    if (v0 eq v1) {
      return true
    }

    v0 match {
      case v0: FeaturedWHNF_equal =>
        v0.feature_equal(v1) match {
          case Some(result) => return result
          case None => {}
        }
      case _ => {}
    }
    v1 match {
      case v1: FeaturedWHNF_equal =>
        v1.feature_equal(v0) match {
          case Some(result) => return result
          case None => {}
        }
      case _ => {}
    }
    v0.toCore().equal_core(v1.toCore())
  }

  def eval(context: Mapping, stack: DebugStack): Value = Delay({
    this.reduce_rec() match {
      case x: FeaturedWHN_eval => x.feature_eval(context, stack)
      case _ => AsExpCached.unapply(this) match {
        case Some(x) => x.feature_eval(context, stack)
        case None => CoreException(stack, Symbols.Exceptions.TypeMismatch_Exp, context, Builtin(Symbols.Eval, List(Quote(context), Quote(this))))
      }
    }
  }, {
    Builtin(Symbols.Eval, List(Quote(context), Quote(this)))
  })

  def app(xs: List[Value], stack: DebugStack): Value = Delay({
    this.reduce_rec() match {
      case x: FeaturedWHNF_app => x.feature_app(xs, stack)
      case _ => AsInterpretedClosureCached.unapply(this) match {
        case Some(x) => x.feature_app(xs, stack)
        case None => CoreException(stack, Symbols.Exceptions.TypeMismatch_Func, Mapping.Empty, ApplyFunc(this, xs))
      }
    }
  }, {
    throw new UnsupportedOperationException("TODO")
  })
}

trait MayNotWHNF {
  def reduce_rec(): WHNF

  def reduce(): MayNotWHNF = this.reduce_rec()

  final def reduce_rec_toCore(): CoreWHNF = this.reduce_rec().toCore()

  def readback(): Exp

  final def unpack_rec(): MayNotWHNF = this match {
    case x: Value => x.do_unpack_rec()
    case _ => this
  }
}

private[lingo] final object AsWHNF {
  def unapply(x: MayNotWHNF): Option[WHNF] = Some(x.reduce_rec())
}

trait WHNF extends MayNotWHNF {
  final override def reduce_rec(): WHNF = this

  final override def readback() = Quote(this)

  def toCore(): CoreWHNF
}

trait FeaturedWHNF_equal extends WHNF {
  def feature_equal(x: Value): Option[Boolean] = None
}

trait FeaturedWHN_eval extends WHNF {
  def feature_eval(context: Mapping, stack: DebugStack): Value
}

trait FeaturedWHNF_app extends WHNF {
  def feature_app(xs: List[Value], stack: DebugStack): Value
}

// GC-ed IO things
trait OpaqueWHNF extends WHNF {

}