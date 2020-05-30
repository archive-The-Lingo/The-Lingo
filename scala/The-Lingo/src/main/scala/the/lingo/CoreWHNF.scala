/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import the.lingo.private_utils.Nat

sealed trait CoreWHNF extends WHNF {
  final override def toCore() = this

  private[lingo] def equal_core(x: CoreWHNF): Boolean
}

private[lingo] final object AsCoreWHNF {
  def unapply(x: WHNF): Option[CoreWHNF] = Some(x.toCore())

  def unapply(x: MayNotWHNF): Option[CoreWHNF] = unapply(x.reduce_rec())
}

final case class Null() extends CoreWHNF {
  private[lingo] override def equal_core(x: CoreWHNF) = x match {
    case Null() => true
    case _ => false
  }
}

final case class Sym(x: Symbol) extends CoreWHNF {
  private[lingo] override def equal_core(y: CoreWHNF) = y match {
    case Sym(y) => x == y
    case _ => false
  }
}

final object Sym {
  implicit def apply(x: Symbol): Sym = Sym(x)

  def apply(x: String): Sym = Sym(Symbol(x))
}

final case class Pair(x: Value, y: Value) extends CoreWHNF {
  private[lingo] override def equal_core(ab: CoreWHNF) = ab match {
    case Pair(a, b) => a.equal_reduce_rec(x) && b.equal_reduce_rec(y)
    case _ => false
  }
}

final case class Tagged(tag: Value, xs: Value) extends CoreWHNF {
  private[lingo] override def equal_core(ab: CoreWHNF) = ab match {
    case Tagged(a, b) => a.equal_reduce_rec(tag) && b.equal_reduce_rec(xs)
    case _ => false
  }
}

final case class Bottom(tag: Value, xs: Value) extends CoreWHNF {
  private[lingo] override def equal_core(ab: CoreWHNF) = ab match {
    case Bottom(a, b) => a.equal_reduce_rec(tag) && b.equal_reduce_rec(xs)
    case _ => false
  }
}

final case class ValueNat(x: Nat) extends CoreWHNF {
  private[lingo] override def equal_core(y: CoreWHNF) = y match {
    case ValueNat(y) => x == y
    case _ => false
  }
}