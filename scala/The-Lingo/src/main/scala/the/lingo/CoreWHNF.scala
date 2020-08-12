/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import the.lingo.utils.{Nat, OnewayWriteFlag}

sealed trait CoreWHNF extends WHNF {
  final override def impl_toCore() = this

  private[lingo] def equal_core(x: CoreWHNF, opaqueFlag: OnewayWriteFlag): Boolean
}

private[lingo] final object AsCoreWHNF {
  def unapply(x: WHNF): Option[CoreWHNF] = Some(x.toCore())

  def unapply(x: MayNotWHNF): Option[CoreWHNF] = unapply(x.reduce_rec())
}

final case object Null extends CoreWHNF {
  private[lingo] override def equal_core(x: CoreWHNF, opaqueFlag: OnewayWriteFlag) = x match {
    case Null => true
    case _ => false
  }

  override def impl_show(implicit showContext: ShowContext): String = "Null"
}

final case class Sym(x: Symbol) extends CoreWHNF {
  override def impl_show(implicit showContext: ShowContext): String = s"Sym('${x.name})"

  private[lingo] override def equal_core(y: CoreWHNF, opaqueFlag: OnewayWriteFlag) = y match {
    case Sym(y) => x == y
    case _ => false
  }
}

final object AsSym {
  def unapply(x: WHNF): Option[Sym] = x match {
    case AsCoreWHNF(x: Sym) => Some(x)
    case _ => None
  }

  def unapply(x: MayNotWHNF): Option[Sym] = x match {
    case AsCoreWHNF(x: Sym) => Some(x)
    case _ => None
  }
}

final object Sym {
  implicit def apply(x: Symbol): Sym = new Sym(x)

  def apply(x: String): Sym = new Sym(Symbol(x))
}

final case class Pair(x: Value, y: Value) extends CoreWHNF {
  private[lingo] override def equal_core(ab: CoreWHNF, opaqueFlag: OnewayWriteFlag) = ab match {
    case Pair(a, b) => a.equal_reduce_rec(x, opaqueFlag) && b.equal_reduce_rec(y, opaqueFlag)
    case _ => false
  }

  override def impl_show(implicit showContext: ShowContext): String = s"Pair(${x.show},${y.show})"
}

// stack is for code-like data
final case class Tagged(tag: Value, xs: Value, stack: Option[DebugStack] = None) extends CoreWHNF {
  private[lingo] override def equal_core(ab: CoreWHNF, opaqueFlag: OnewayWriteFlag) = ab match {
    case Tagged(a, b) => a.equal_reduce_rec(tag, opaqueFlag) && b.equal_reduce_rec(xs, opaqueFlag)
    case _ => false
  }

  override def impl_show(implicit showContext: ShowContext): String =
    s"Tagged(${tag.show},${xs.show}${
      stack match {
        case Some(x) => s",${x.show}"
        case None => ""
      }
    })"
}

final object Tagged {
  def unapply(x: Tagged): Option[(Value, Value)] = x match {
    case x: Tagged => Some((x.tag, x.xs))
  }

  def unapply(x: CoreWHNF): Option[(Value, Value)] = x match {
    case Tagged(a, b) => Some((a, b))
    case _ => None
  }
}

final case class ValueException(tag: Value, xs: Value) extends CoreWHNF {
  private[lingo] override def equal_core(ab: CoreWHNF, opaqueFlag: OnewayWriteFlag) = ab match {
    case ValueException(a, b) => a.equal_reduce_rec(tag, opaqueFlag) && b.equal_reduce_rec(xs, opaqueFlag)
    case _ => false
  }

  override def impl_show(implicit showContext: ShowContext): String = s"ValueException(${tag.show},${xs.show})"
}

final case class ValueNat(x: Nat) extends CoreWHNF {
  private[lingo] override def equal_core(y: CoreWHNF, opaqueFlag: OnewayWriteFlag) = y match {
    case ValueNat(y) => x == y
    case _ => false
  }

  override def impl_show(implicit showContext: ShowContext): String = s"Nat(${x.toString()})"
}

private final object NatUtils {
  def nat2booleanList(x: Nat): List[Boolean] =
    (0 until x.bitLength).toList.map(x.testBit)

  def booleanList2nat(xs: List[Boolean]): Nat =
    xs.zipWithIndex.foldLeft(Nat(0))((n, x) => if (x._1) n.setBit(x._2) else n)
}
