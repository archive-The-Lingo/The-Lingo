package lingo.corefp

import lingo.corefp.utils.Nat

object ValueNat {
  def apply(x: Nat): Value = todo()

  def unapply(x: Value): Option[Nat] = Value.getComponentOrAddOption(x, {
    x match {
      case TaggedSeq(Atoms.Tags.BinaryNat, ValueListSeq(ValueBoolean(_))) => todo()
      case _ => None
    }
  })
}

private object NatUtils {
  def nat2booleanList(x: Nat): List[Boolean] =
    (0 until x.bitLength).toList.map(x.testBit)

  def booleanList2nat(xs: List[Boolean]): Nat =
    xs.zipWithIndex.foldLeft(Nat(0))((n, x) => if (x._1) n.setBit(x._2) else n)
}

private object ValueListBoolean {
  private def traverse[T](xs: List[Option[T]]): Option[List[T]] = xs match {
    case Nil => Some(Nil)
    case Some(head) :: tail => traverse(tail).map(head :: _)
    case None :: _ => None
  }

  def unapply(x: Value): Option[List[Boolean]] = ValueList.unapply(x).flatMap(xs => traverse(xs.map(ValueBoolean.unapply)))
}