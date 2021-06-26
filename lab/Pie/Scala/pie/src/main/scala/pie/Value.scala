package pie

import scala.collection.immutable.HashMap

type Identifier = Symbol

type Nat = Int

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

case class Add1(x: NaturalNumber | Neu) extends NaturalNumber

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

sealed trait Closure extends Value {
  // It is not straightforward to store type information in closures to determine level since closures' type information is Type*Closure
  // Every closure will have a corresponding `The`, from which level can be determined
  override def level: Nat = throw new IllegalStateException("Closure's level is unknown")
}

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

case class Pair(car: Value, cdr: Value) extends Value {
  override def level: Nat = car.level.max(cdr.level)
}

case class Eq(t: Type, from: Value, to: Value) extends Type {
  override def level: Nat = t.level + 1
}

case class Same(x: Value) extends Value

case class Atom(x: Symbol) extends Value

case object AtomT extends Type

def checkNat(x: Nat): Nat = {
  if (x < 0) {
    throw new IllegalArgumentException("")
  }
  x
}

type Maybe[T] = Either[String, T]

case class Definitions(x: HashMap[Identifier, (Type, Value)]) {
  def get(id: Identifier): Maybe[(Type, Value)] = x.get(id) match {
    case Some(v) => Right(v)
    case None => Left(s"Definition not found $id")
  }

  def getType(id: Identifier): Maybe[Type] = get(id).map(_._1)

  def getValue(id: Identifier): Maybe[Value] = get(id).map(_._2)
}

sealed trait Exp {
  def synth(Γ: Definitions): Maybe[The]

  def check(Γ: Definitions, t: Type): Maybe[Exp] = throw new Exception("WIP")

  def check(Γ: Definitions, t: Value): Maybe[Exp] = t match {
    case x: Type => this.check(Γ, x)
    case _ => Left(s"Not a type $t")
  }

  def autoLevel(Γ: Definitions): Maybe[Nat] = this.eval(Γ) match {
    case Right(v) => Right(v.level)
    case Left(s) => this.synth(Γ).flatMap(_.autoLevel(Γ)) match {
      case Right(v) => Right(checkNat(v - 1))
      case Left(s2) => Left(s"find level failed $s $s2")
    }
  }

  def manualLevel(Γ: Definitions): Maybe[Nat]

  def eval(env: Definitions): Maybe[Value]
}

case class The(valueType: Exp, value: Exp) extends Exp {
  // `type.level = value.level + 1` always holds
  override def synth(Γ: Definitions): Maybe[The] = for {
    l <- valueType.autoLevel(Γ)
    t <- valueType.check(Γ, U(l))
    t0 <- t.eval(Γ)
    e <- value.check(Γ, t0)
  } yield The(t, e)

  // sometimes we don't have value information
  override def manualLevel(Γ: Definitions): Maybe[Nat] = valueType.autoLevel(Γ).map(_ - 1).map(checkNat)

  override def eval(env: Definitions): Maybe[Value] = value.eval(env)
}

case class Lambda(argument: Identifier, body: Exp) extends Exp {
  override def manualLevel(Γ: Definitions): Maybe[Nat] = body.autoLevel(Γ)

  override def eval(env: Definitions): Maybe[Value] = Right(PieClosure(env, argument, body))

  override def synth(Γ: Definitions): Maybe[The] = throw new Exception("WIP")
}

case class Variable(x: Identifier) extends Exp {
  override def eval(env: Definitions): Maybe[Value] = env.getValue(x)

  override def manualLevel(Γ: Definitions): Maybe[Nat] = this.eval(Γ).map(_.level)

  override def synth(Γ: Definitions): Maybe[The] = throw new Exception("WIP")
}

case class ElimNat(target: Exp, motive: Exp, base: Exp, step: Exp) extends Exp {
  override def manualLevel(Γ: Definitions): Maybe[Nat] = for {
    t <- target.autoLevel(Γ)
    m <- motive.autoLevel(Γ)
    b <- base.autoLevel(Γ)
    s <- step.autoLevel(Γ)
  } yield t.max(m).max(b).max(s)

  override def eval(env: Definitions): Maybe[Value] = throw new Exception("WIP")

  override def synth(Γ: Definitions): Maybe[The] = throw new Exception("WIP")
}

case class NeuElimNat(target: Neu, motive: Value, base: Value, step: Value) extends Neu {
  override def level: Nat = target.level.max(motive.level).max(base.level).max(step.level) // todo: check me
}

case class ElimAbsurd(target: Exp, motive: Exp) extends Exp {
  override def manualLevel(Γ: Definitions): Maybe[Nat] = for {
    t <- target.autoLevel(Γ)
    m <- motive.autoLevel(Γ)
  } yield t.max(m)

  override def eval(env: Definitions): Maybe[Value] = throw new Exception("WIP")

  override def synth(Γ: Definitions): Maybe[The] = throw new Exception("WIP")
}

case class NeuElimAbsurd(target: Neu, motive: Value) extends Neu {
  override def level: Nat = target.level.max(motive.level) // todo: check me
}

case class RecNat(t: Exp, target: Exp, base: Exp, step: Exp) extends Exp {
  override def eval(env: Definitions): Maybe[Value] = throw new Exception("WIP")

  override def manualLevel(Γ: Definitions): Maybe[Nat] = for {
    c <- t.autoLevel(Γ)
    t <- target.autoLevel(Γ)
    b <- base.autoLevel(Γ)
    s <- step.autoLevel(Γ)
  } yield c.max(t).max(b).max(s)

  override def synth(Γ: Definitions): Maybe[The] = throw new Exception("WIP")
}

case class NeuRecNat(t: Value, target: Neu, base: Value, step: Value) extends Neu {
  override def level: Nat = t.level.max(target.level).max(base.level).max(step.level) // todo: check me
}

case class Quote(symbol: Symbol) extends Exp {
  private val value = Atom(symbol)

  override def manualLevel(Γ: Definitions): Maybe[Nat] = Right(0)

  override def eval(env: Definitions): Maybe[Value] = Right(value)

  override def synth(Γ: Definitions): Maybe[The] = Right(The(EmbeddedValue(U1, AtomT), this))
}

case class EmbeddedValue(t: Type, v: Value) extends Exp {
  override def manualLevel(Γ: Definitions): Maybe[Nat] = Right(t.level)

  override def eval(env: Definitions): Maybe[Value] = Right(v)

  override def synth(Γ: Definitions): Maybe[The] = Right(The(EmbeddedValue(U(t.level), t), this))
}

// car can't be a value
case class Car(x: Exp) extends Exp {
  override def manualLevel(Γ: Definitions): Maybe[Nat] = x.autoLevel(Γ)

  override def eval(env: Definitions): Maybe[Value] = x.eval(env) flatMap {
    case Pair(a, d) => Right(a)
    case v => Left(s"not a pair $v")
  }

  override def synth(Γ: Definitions): Maybe[The] = throw new Exception("WIP")
}

case class Cdr(x: Exp) extends Exp {
  override def manualLevel(Γ: Definitions): Maybe[Nat] = x.autoLevel(Γ)

  override def eval(env: Definitions): Maybe[Value] = x.eval(env) flatMap {
    case Pair(a, d) => Right(d)
    case v => Left(s"not a pair $v")
  }

  override def synth(Γ: Definitions): Maybe[The] = throw new Exception("WIP")
}

case class Apply(f: Exp, x: Exp) extends Exp {
  override def manualLevel(Γ: Definitions): Maybe[Nat] = x.autoLevel(Γ)

  override def eval(env: Definitions): Maybe[Value] = throw new Exception("WIP")

  override def synth(Γ: Definitions): Maybe[The] = throw new Exception("WIP")
}
