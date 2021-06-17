package pie

import scala.collection.immutable.HashMap

type Identifier = Symbol

type Nat = Int

type Maybe[T] = Either[String, T]

case class Definitions(x: HashMap[Identifier, (Type, Value)]) {
  def toContext: Context = Context(x.map(_ match { case (id, (t, v)) => (id, (t, Some(v))) }))

  def get(id: Identifier): Maybe[(Type, Value)] = x.get(id) match {
    case Some(v) => Right(v)
    case None => Left(s"Definition not found $id")
  }

  def getType(id: Identifier): Maybe[Type] = get(id).map(_._1)

  def getValue(id: Identifier): Maybe[Value] = get(id).map(_._2)
}

case class Context(x: HashMap[Identifier, (Type, Option[Value])]) {
  def toDefinitions: Definitions = throw new Exception("WIP")

  def get(id: Identifier): Maybe[(Type, Option[Value])] = x.get(id) match {
    case Some(v) => Right(v)
    case None => Left(s"Definition not found $id")
  }

  def getType(id: Identifier): Maybe[Type] = get(id).map(_._1)

  def getOptionValue(id: Identifier): Maybe[Option[Value]] = get(id).map(_._2)
}

val globalDefinitions: Definitions = Definitions(HashMap((Symbol("Absurd"), (U(0), Absurd))))

sealed trait Exp {
  def synth(Γ: Context): Maybe[The] = throw new Exception("WIP")

  def check(Γ: Context, t: Type): Maybe[Exp] = throw new Exception("WIP")

  def check(Γ: Context, t: Value): Maybe[Exp] = t match {
    case x: Type => this.check(Γ, x)
    case _ => Left(s"Not a type $t")
  }

  def level(Γ: Context): Nat = throw new Exception("WIP")

  def eval(Γ: Context): Maybe[Value] = this.eval(Γ.toDefinitions)

  def eval(env: Definitions): Maybe[Value] = throw new Exception("WIP")
}

case class The(valueType: Exp, value: Exp) extends Exp {
  override def synth(Γ: Context): Maybe[The] = for {
    t <- valueType.check(Γ, U(valueType.level(Γ)))
    t0 <- t.eval(Γ)
    e <- value.check(Γ, t0)
  } yield The(t, e)

  override def level(Γ: Context): Nat = valueType.level(Γ).max(value.level(Γ))
}

case class Lambda(argument: Identifier, body: Exp) extends Exp {
  override def level(Γ: Context): Nat = body.level(Γ)
}

case class Variable(x: Identifier) extends Exp {
  override def eval(env: Definitions): Maybe[Value] = env.getValue(x)

  override def level(Γ: Context): Nat = throw new Exception("WIP")
}

case class ElimNat(target: Exp, motive: Exp, base: Exp, step: Exp) extends Exp {
  override def level(Γ: Context): Nat = target.level(Γ).max(motive.level(Γ)).max(base.level(Γ)).max(step.level(Γ))
}

case class ElimAbsurd(target: Exp, motive: Exp) extends Exp {
  override def level(Γ: Context): Nat = target.level(Γ).max(motive.level(Γ))
}
