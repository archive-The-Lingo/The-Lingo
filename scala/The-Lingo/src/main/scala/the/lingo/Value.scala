/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import the.lingo.utils.ByReferenceWrapper.Implicits._
import the.lingo.utils.{ByReferenceWrapper, MutableBox, Nat, OnewayWriteFlag}

import scala.collection.{immutable, mutable}

final object Value {

  final object Implicits {
    implicit def implicitPackWHNF[A <: WHNF](x: A): Value = Value(x)

    implicit def implicitPackWHNFList[A <: WHNF](xs: List[A]): List[Value] = xs.map {
      Value(_)
    }
  }

  private[lingo] def cached_option_as[A <: WHNF](f: WHNF => Option[A]): Value => Option[A] =
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

  import Value.Implicits._

  override def impl_show(implicit showContext: ShowContext): String = x.show(showContext)

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
    val result = ptr.x.reduce_rec()
    ptr.smart_maybe_write(result)
    result
  }

  override def reduce() = {
    val ptr = this.unpack_rec_to_single_pack()
    val result = ptr.x.reduce()
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
    val history: mutable.HashSet[ByReferenceWrapper[Value]] = new mutable.HashSet()
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

  /** opaqueFlag only make sense when the result is true  */
  def equal_reduce_rec(arg: Value, opaqueFlag: OnewayWriteFlag = new OnewayWriteFlag): Boolean = {
    val (self, x) = (this.unpack_rec(), arg.unpack_rec())
    if (self eq x) {
      return true
    }
    val (v0, v1) = (self.reduce_rec(), x.reduce_rec())
    if (v0 eq v1) {
      return true
    }

    val innerOpaqueFlag = new OnewayWriteFlag
    innerOpaqueFlag.or(v0.isInstanceOf[OpaqueWHNF] || v1.isInstanceOf[OpaqueWHNF])
    val finalResult = (v0 match {
      case v0: FeaturedWHNF_equal =>
        v0.feature_equal(v1, innerOpaqueFlag) match {
          case Some(result) => Some(result)
          case None => None
        }
      case _ => None
    }) orElse {
      v1 match {
        case v1: FeaturedWHNF_equal =>
          v1.feature_equal(v0, innerOpaqueFlag) match {
            case Some(result) => Some(result)
            case None => None
          }
        case _ => None
      }
    } getOrElse {
      v0.toCore().equal_core(v1.toCore(), innerOpaqueFlag)
    }
    opaqueFlag.or(innerOpaqueFlag.get)
    if (finalResult && innerOpaqueFlag.get) {
      // TODO: write Value instead of WHNF
      arg.smart_maybe_write(v0)
    }
    finalResult
  }

  def eval(context: Mapping = Mapping.Empty, stack: DebugStack = DebugStack.Empty): Value = Delay({
    this match {
      case AsWHNF(x: FeaturedWHN_eval) => x.feature_eval(context, stack)
      case AsExpCached(x) => x.feature_eval(context, stack)
      case _ => CoreException(stack, Symbols.CoreExceptions.TypeMismatch_Exp, context, Builtin(Symbols.Builtins.Eval, List(Quote(context), Quote(this))))
    }
  }, {
    Builtin(Symbols.Builtins.Eval, List(Quote(context), Quote(this)))
  })

  def eval_callByName(context0: => Mapping = Mapping.Empty, stack0: => DebugStack = DebugStack.Empty): Value = Delay({
    lazy val context = context0
    lazy val stack = stack0
    this match {
      case AsWHNF(x: FeaturedWHN_eval) => x.feature_eval(context, stack)
      case AsExpCached(x) => x.feature_eval(context, stack)
      case _ => CoreException(stack, Symbols.CoreExceptions.TypeMismatch_Exp, context, Builtin(Symbols.Builtins.Eval, List(Quote(context), Quote(this))))
    }
  }, {
    Builtin(Symbols.Builtins.Eval, List(Quote(context0), Quote(this)))
  })

  def app(xs: List[Value], stack: DebugStack = DebugStack.Empty): Value = Delay({
    this match {
      case AsWHNF(x: FeaturedWHNF_app) => x.feature_app(xs, stack)
      case AsInterpretedClosureCached(x) => x.feature_app(xs, stack)
      case _ => CoreException(stack, Symbols.CoreExceptions.TypeMismatch_Func, Mapping.Empty, ApplyFunc(this, xs))
    }
  }, {
    ApplyFunc(this, xs)
  })
}

final case class ShowContext private[lingo](val parents: immutable.HashSet[ByReferenceWrapper[Showable]], val context: immutable.HashMap[ByReferenceWrapper[Showable], Nat], val count: MutableBox[Nat]) {
  private[lingo] def newId: Nat = {
    count.synchronized {
      val x = count.get
      count.update(x.succ)
      x
    }
  }
}

private final object ShowContext {
  def apply() = new ShowContext(new immutable.HashSet(), new immutable.HashMap(), new MutableBox(Nat.Zero))
}

trait Showable {
  private[lingo] def impl_show(implicit showContext: ShowContext): String

  final def show(implicit showContext: ShowContext): String = this match {
    case self: MayNotWHNF => {
      val parents = showContext.parents
      val context = showContext.context
      context.get(self) match {
        case Some(x) => "#" + x.toString()
        case None => {
          val innerParents = parents.incl(self)
          val innerContext = {
            if (parents(self)) {
              context.updated(self, showContext.newId)
            } else {
              context
            }
          }
          assert(innerParents(self)) // TODO: FIX ME
          self.impl_show(new ShowContext(innerParents, innerContext, showContext.count))
        }
      }
    }
    case _ => this.impl_show(showContext)
  }

  final override def toString(): String = this.show(ShowContext())
}

trait MayNotWHNF extends Showable {

  def reduce_rec(): WHNF

  def reduce(): MayNotWHNF = this.reduce_rec()

  final def reduce_rec_toCore(): CoreWHNF = this.reduce_rec().toCore()

  /** only for non-WHNF */
  def readback(): Exp

  final def unpack_rec(): MayNotWHNF = this match {
    case x: Value => x.do_unpack_rec()
    case _ => this
  }

}

final object Showable {

  final object Implicits {

    implicit class ShowXs[A <: Showable](xs: List[A]) {
      def showXs(implicit showContext: ShowContext): String = xs.map(_.show).mkString(",")
    }

    implicit class ShowList[A <: Showable](x: List[A]) extends Showable {
      override def impl_show(implicit showContext: ShowContext): String = s"List(${x.showXs})"
    }

    implicit class ShowOption[A <: Showable](x: Option[A]) extends Showable {
      override def impl_show(implicit showContext: ShowContext): String = x match {
        case Some(x) => s"Some(${x.show})"
        case None => "None"
      }
    }

    implicit class ShowTuple2[A <: Showable, B <: Showable](x: (A, B)) extends Showable {
      override def impl_show(implicit showContext: ShowContext): String = s"(${x._1.show},${x._2.show})"
    }

    implicit class ShowTuple2List[A <: Showable, B <: Showable](xs: List[(A, B)]) extends Showable {
      override def impl_show(implicit showContext: ShowContext): String = xs.map(ShowTuple2).show
    }

  }

}

private[lingo] final object AsWHNF {
  def unapply(x: MayNotWHNF): Option[WHNF] = Some(x.reduce_rec())
}

trait WHNF extends MayNotWHNF {

  import Value.Implicits._

  final override def reduce_rec(): WHNF = this

  final override def readback() = Quote(this)

  private[lingo] def impl_toCore(): CoreWHNF

  final private lazy val coreForm = this.impl_toCore()

  final def toCore(): CoreWHNF = coreForm
}

trait FeaturedWHNF_equal extends WHNF {
  def feature_equal(x: Value, opaqueFlag: OnewayWriteFlag): Option[Boolean] = None
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