package pie

import scala.collection.immutable.HashMap

private def impossible[A]:A = throw new IllegalStateException("impossible!!!")

private def add1(x: Value): Value = x match {
  case x: NaturalNumber => x.add1
  case _ => impossible
}

val prelude: Definitions = Definitions(HashMap(
  (Symbol("Absurd"), (U(1), Absurd)),
  (Symbol("add1"), (SimplePi(NatT, NatT), PrimitiveClosure(add1))),
))
