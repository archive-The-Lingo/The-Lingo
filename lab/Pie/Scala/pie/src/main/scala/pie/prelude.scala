package pie

import scala.collection.immutable.HashMap

private def impossible[A]:A = throw new IllegalStateException("impossible!!!")

private def add1(x: Value): Value = x match {
  case x: Neu => throw new Exception("WIP")
  case x: NaturalNumber => x.add1
  case _ => impossible
}

val prelude: Definitions = Definitions(HashMap(
  (Symbol("Absurd"), (U1, AbsurdT)),
  (Symbol("Nat"), (U1, NatT)),
  (Symbol("add1"), (SimplePi(NatT, NatT), PrimitiveClosure(add1))),
  (Symbol("zero"), (NatT, NaturalNumber.zero))
))
