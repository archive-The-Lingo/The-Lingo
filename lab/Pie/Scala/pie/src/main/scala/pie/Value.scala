package pie

sealed trait Value {
  def level: Nat = 0
}

sealed trait Type extends Value {
  override def level: Nat = 1
}

// consider U(0) = Any, U(1) = Set (Set in Agda)
case class U(x: Nat) extends Type {
  override def level: Nat = x + 1
}

val U1 = U(1)

sealed trait NaturalNumber extends Value {
  def add1: Add1 = Add1(this)
}

case object Zero extends NaturalNumber

case class Add1(x: Value) extends NaturalNumber

object NaturalNumber {
  val zero: NaturalNumber = Zero

  def apply(x: Nat): NaturalNumber = if (x < 0) {
    throw new IllegalArgumentException("Not a natural number")
  } else if (x == 0) {
    Zero
  } else {
    NaturalNumber(x - 1).add1
  }
}

case object NatT extends Type

case object AbsurdT extends Type

sealed trait Closure extends Value

case class PieClosure(env: Definitions, x: Identifier, body: Exp) extends Closure

case class PrimitiveClosure(x: Value => Value) extends Closure

sealed trait Neu extends Value

case class NeuVar(t: Type, name: Symbol, id: Nat) extends Neu

object NeuVar {
  private var neuCount: Nat = 0

  def apply(t: Type, name: Symbol): NeuVar = this.synchronized {
    val result = NeuVar(t, name, neuCount)
    neuCount = neuCount + 1
    result
  }
}

case class Pi(domain: Type, range: Closure /*: Type -> Type*/) extends Type {
  override def level: Nat = domain.level + 1 + 1 // todo: check me
}

object SimplePi {
  def apply(domain: Type, range: Type): Pi = Pi(domain, PrimitiveClosure(_ => range))
}

case class Sigma(carType: Type, cdrType: Closure /*: Type -> Type*/) extends Type {
  override def level: Nat = carType.level + 1 + 1 // todo: check me
}

case class Pair(car: Value, cdr: Value) extends Value

case class Eq(t: Type, from: Value, to: Value) extends Type {
  override def level: Nat = t.level + 1
}

case class Same(x: Value) extends Value

case class Atom(x: Symbol) extends Value

case object AtomT extends Type