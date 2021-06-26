package pie

import scala.collection.immutable.HashMap

type Identifier = Symbol

type Nat = Int

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

case class ElimAbsurd(target: Exp, motive: Exp) extends Exp {
  override def manualLevel(Γ: Definitions): Maybe[Nat] = for {
    t <- target.autoLevel(Γ)
    m <- motive.autoLevel(Γ)
  } yield t.max(m)

  override def eval(env: Definitions): Maybe[Value] = throw new Exception("WIP")

  override def synth(Γ: Definitions): Maybe[The] = throw new Exception("WIP")
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

  override def eval(env: Definitions): Maybe[Value] = throw new Exception("WIP")

  override def synth(Γ: Definitions): Maybe[The] = throw new Exception("WIP")
}

case class Cdr(x: Exp) extends Exp {
  override def manualLevel(Γ: Definitions): Maybe[Nat] = x.autoLevel(Γ)

  override def eval(env: Definitions): Maybe[Value] = throw new Exception("WIP")

  override def synth(Γ: Definitions): Maybe[The] = throw new Exception("WIP")
}

case class Apply(f: Exp, x: Exp) extends Exp {
  override def manualLevel(Γ: Definitions): Maybe[Nat] = x.autoLevel(Γ)

  override def eval(env: Definitions): Maybe[Value] = throw new Exception("WIP")

  override def synth(Γ: Definitions): Maybe[The] = throw new Exception("WIP")
}