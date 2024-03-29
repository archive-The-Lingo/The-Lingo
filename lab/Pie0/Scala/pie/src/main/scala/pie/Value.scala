package pie

// Core Pie

import scala.collection.immutable.HashMap

private def todo[A]: A = throw new Exception("WIP")

type Identifier = Symbol

type Nat = Int

sealed trait Value {
  def level: Nat = 0

  final def alpha_equivalent(ctx: Definitions, t: Type, y: Value): Boolean = this.readback(ctx, t).alpha_equivalent(ctx, y.readback(ctx, t))

  def readback(ctx: Definitions, t: Type): Exp = todo
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

  def apply(x: Value): Maybe[Value] = todo
}

case class PieClosure(env: UntyppedDefinitions, x: Identifier, body: Exp) extends Closure {
  override def apply(arg: Value): Maybe[Value] = body.eval(env.extend(x, arg))
}

case class PrimitiveClosure(x: Value => Value) extends Closure {
  override def apply(arg: Value): Maybe[Value] = Right(x(arg))
}

sealed trait Neu extends Value {
  def getType: Type
}

case class NeuVar(t: Type, name: Symbol, id: Nat) extends Neu {
  override def getType: Type = this.t
}

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

case class Definitions(inner: HashMap[Identifier, (Type, Value)]) {
  def get(id: Identifier): Maybe[(Type, Value)] = inner.get(id) match {
    case Some(v) => Right(v)
    case None => Left(s"Definition not found $id")
  }

  def getType(id: Identifier): Maybe[Type] = get(id).map(_._1)

  def getValue(id: Identifier): Maybe[Value] = get(id).map(_._2)

  def extend(id: Identifier, t: Type, x: Value): Definitions = {
    val p: (Identifier, (Type, Value)) = (id, (t, x))
    Definitions(inner + p)
  }

  def untypped: UntyppedDefinitions = UntyppedDefinitions(inner.map({ case (id, (t, v)) => (id, v) }))
}

case class UntyppedDefinitions(inner: HashMap[Identifier, Value]) {
  def getValue(id: Identifier): Maybe[Value] = inner.get(id) match {
    case Some(v) => Right(v)
    case None => Left(s"Definition not found $id")
  }

  def extend(id: Identifier, x: Value): UntyppedDefinitions = {
    val p: (Identifier, Value) = (id, x)
    UntyppedDefinitions(inner + p)
  }
}

sealed trait Exp {
  def synth(Γ: Definitions): Maybe[Typed]

  def check(Γ: Definitions, t: Type): Maybe[Value] = todo

  def check(Γ: Definitions, t: Value): Maybe[Value] = t match {
    case x: Type => this.check(Γ, x)
    case _ => Left(s"Not a type $t")
  }

  final def autoLevel(Γ: Definitions): Maybe[Nat] = this.eval(Γ) match {
    case Right(v) => Right(v.level)
    case Left(s) => this.synth(Γ).map(_.level) match {
      case Right(v) => Right(checkNat(v - 1))
      case Left(s2) => Left(s"find level failed $this - $s - $s2")
    }
  }

  def manualLevel(Γ: Definitions): Maybe[Nat]

  def eval(env: UntyppedDefinitions): Maybe[Value]

  final def eval(Γ: Definitions): Maybe[Value] = eval(Γ.untypped)

  private[pie] final def levelFallback1(Γ: Definitions): Nat = try {
    this.autoLevel(Γ) match {
      case Right(v) => v
      case Left(_) => 1
    }
  } catch {
    case e: IllegalStateException => 1
  }

  def alpha_equivalent(ctx: Definitions, y: Exp): Boolean = todo
}

case class The(valueType: Exp, value: Exp) extends Exp {
  // `type.level = value.level + 1` always holds
  override def synth(Γ: Definitions): Maybe[Typed] = for {
    l <- Right(valueType.levelFallback1(Γ))
    t0 <- valueType.check(Γ, U(l)) // so t0 must be a Type
    v <- value.check(Γ, t0)
  } yield Typed(t0.asInstanceOf[Type], v)

  // sometimes we don't have value information
  override def manualLevel(Γ: Definitions): Maybe[Nat] = valueType.autoLevel(Γ).map(_ - 1).map(checkNat)

  override def eval(env: UntyppedDefinitions): Maybe[Value] = value.eval(env)
}

case class Typed(t: Type, x: Value) {
  def level: Nat = t.level - 1 // todo: check me - why `t`? why not `x`?
}

case class Lambda(argument: Identifier, body: Exp) extends Exp {
  override def manualLevel(Γ: Definitions): Maybe[Nat] = body.autoLevel(Γ)

  override def eval(env: UntyppedDefinitions): Maybe[Value] = Right(PieClosure(env, argument, body))

  override def synth(Γ: Definitions): Maybe[Typed] = Left("synth Lambda is not supported")
}

case class Variable(x: Identifier) extends Exp {
  override def eval(env: UntyppedDefinitions): Maybe[Value] = env.getValue(x)

  override def manualLevel(Γ: Definitions): Maybe[Nat] = this.eval(Γ).map(_.level)

  override def synth(Γ: Definitions): Maybe[Typed] = Γ.get(x) map { case (t, v) => Typed(t, v) }
}

case class ElimNat(target: Exp, motive: Exp, base: Exp, step: Exp) extends Exp {
  override def manualLevel(Γ: Definitions): Maybe[Nat] = for {
    t <- target.autoLevel(Γ)
    m <- motive.autoLevel(Γ)
    b <- base.autoLevel(Γ)
    s <- step.autoLevel(Γ)
  } yield t.max(m).max(b).max(s)

  override def eval(env: UntyppedDefinitions): Maybe[Value] = todo

  override def synth(Γ: Definitions): Maybe[Typed] = todo
}

case class NeuElimNat(target: Neu, motive: Value, base: Value, step: Value) extends Neu {
  override def level: Nat = target.level.max(motive.level).max(base.level).max(step.level) // todo: check me

  override def getType: Type = todo
}

case class ElimAbsurd(target: Exp, motive: Exp) extends Exp {
  override def manualLevel(Γ: Definitions): Maybe[Nat] = for {
    t <- target.autoLevel(Γ)
    m <- motive.autoLevel(Γ)
  } yield t.max(m)

  override def eval(env: UntyppedDefinitions): Maybe[Value] = target.eval(env) flatMap {
    case target: Neu => motive.eval(env) flatMap {
      case t: Type => Right(NeuElimAbsurd(target, t))
      case t => Left(s"Not a type $t")
    }
    case x => Left(s"Absurd exists?! $x")
  }

  override def synth(Γ: Definitions): Maybe[Typed] = this.eval(Γ).map(Typed(AbsurdT, _))
}

case class NeuElimAbsurd(target: Neu, motive: Type) extends Neu {
  override def level: Nat = target.level.max(motive.level) // todo: check me

  override def getType: Type = AbsurdT
}

case class RecNat(t: Exp, target: Exp, base: Exp, step: Exp) extends Exp {

  override def eval(env: UntyppedDefinitions): Maybe[Value] = todo

  override def manualLevel(Γ: Definitions): Maybe[Nat] = for {
    c <- t.autoLevel(Γ)
    t <- target.autoLevel(Γ)
    b <- base.autoLevel(Γ)
    s <- step.autoLevel(Γ)
  } yield c.max(t).max(b).max(s)

  override def synth(Γ: Definitions): Maybe[Typed] = for {
    l <- Right(t.levelFallback1(Γ))
    t1 <- t.check(Γ, U(l)) // so t1: Type
    v <- this.eval(Γ)
  } yield Typed(t1.asInstanceOf[Type], v)
}

case class NeuRecNat(t: Type, target: Neu, base: Value, step: Value) extends Neu {
  override def level: Nat = t.level.max(target.level).max(base.level).max(step.level) // todo: check me

  override def getType: Type = t
}

case class Quote(symbol: Symbol) extends Exp {
  private val value = Atom(symbol)

  override def manualLevel(Γ: Definitions): Maybe[Nat] = Right(0)

  override def eval(env: UntyppedDefinitions): Maybe[Value] = Right(value)

  override def synth(Γ: Definitions): Maybe[Typed] = Right(Typed(AtomT, value))
}

// car can't be a value
case class Car(x: Exp) extends Exp {
  override def manualLevel(Γ: Definitions): Maybe[Nat] = x.autoLevel(Γ)

  override def eval(env: UntyppedDefinitions): Maybe[Value] = x.eval(env) flatMap {
    case Pair(a, d) => Right(a)
    case n: Neu => Right(NeuCar(n))
    case v => Left(s"not a pair $v")
  }

  override def synth(Γ: Definitions): Maybe[Typed] = todo
}

case class NeuCar(target: Neu) extends Neu {
  override def level: Nat = target.level

  override def getType: Type = target.getType match {
    case Sigma(a, d) => a
    case _ => throw IllegalStateException(s"not Sigma $target")
  }
}

case class Cdr(x: Exp) extends Exp {
  override def manualLevel(Γ: Definitions): Maybe[Nat] = x.autoLevel(Γ)

  override def eval(env: UntyppedDefinitions): Maybe[Value] = x.eval(env) flatMap {
    case Pair(a, d) => Right(d)
    case n: Neu => Right(NeuCdr(n))
    case v => Left(s"not a pair $v")
  }

  override def synth(Γ: Definitions): Maybe[Typed] = todo
}

case class NeuCdr(target: Neu) extends Neu {
  override def level: Nat = target.level

  override def getType: Type = target.getType match {
    case Sigma(a, d) => d.apply(NeuCar(target)) match {
      case Right(t: Type) => t
      case otherwise => throw IllegalStateException(s"not Type $otherwise")
    }
    case _ => throw IllegalStateException(s"not Sigma $target")
  }
}

case class Apply(f: Exp, x: Exp) extends Exp {
  override def manualLevel(Γ: Definitions): Maybe[Nat] = x.autoLevel(Γ)

  override def eval(env: UntyppedDefinitions): Maybe[Value] = for {
    f <- f.eval(env)
    x <- x.eval(env)
    r <- (f, x) match {
      case (f: Neu, x) => Right(NeuApply(f, x))
      case _ => todo
    }
  } yield r

  override def synth(Γ: Definitions): Maybe[Typed] = todo
}

case class NeuApply(f: Neu, x: Value) extends Neu {
  override def level: Nat = f.level.max(x.level)

  override def getType: Type = f.getType match {
    case Pi(_, range) => range.apply(x) match {
      // todo: what if t is Neu?
      case Right(t: Type) => t
      case otherwise => throw IllegalStateException(s"not Type $otherwise")
    }
    case _ => throw IllegalStateException(s"not Pi $f")
  }
}
