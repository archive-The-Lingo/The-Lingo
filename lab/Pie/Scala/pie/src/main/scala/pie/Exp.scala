package pie

import scala.collection.immutable.HashMap

type Identifier = Symbol

type Nat = Int

case class Definitions(x: HashMap[Identifier, (Type, Value)]) {
  def toContext: Context = Context(x.map(_ match { case (id, (t, v)) => (id, (t, Some(v))) }))
}

case class Context(x: HashMap[Identifier, (Type, Option[Value])]) {
  def toDefinitions: Definitions = throw new Exception("WIP")
}

val globalDefinitions: Definitions = Definitions(HashMap((Symbol("Absurd"), (U(0), Absurd))))

sealed trait Exp {
  def synth(Γ: Context): Option[The] = throw new Exception("WIP")

  def check(Γ: Context, t: Type): Option[Exp] = throw new Exception("WIP")

  def check(Γ: Context, t: Value): Option[Exp] = throw new Exception("WIP")

  def level(Γ: Context): Nat = throw new Exception("WIP")

  def eval(Γ: Context): Value = this.eval(Γ.toDefinitions)

  def eval(env: Definitions): Value = throw new Exception("WIP")
}

case class The(valueType: Exp, value: Exp) extends Exp {
  override def synth(Γ: Context): Option[The] = for {
    t <- valueType.check(Γ, U(valueType.level(Γ)))
    e <- value.check(Γ, t.eval(Γ))
  } yield The(t, e)
}

case class Lambda(argument: Identifier, body: Exp) extends Exp

case class Variable(x: Identifier) extends Exp

case class ElimNat(target: Exp, motive: Exp, base: Exp, step: Exp) extends Exp

case class ElimAbsurd(target: Exp, motive: Exp) extends Exp
