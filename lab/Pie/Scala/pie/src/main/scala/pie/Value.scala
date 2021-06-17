package pie

sealed trait Value {
  def level: Nat = 0
}

sealed trait Type extends Value {
  override def level: Nat = 1
}

case class U(x: Nat) extends Type {
  override def level: Nat = x + 1
}

case class NaturalNumber(x: Nat) extends Value

case object Absurd extends Type

case class Closure(env: Definitions, x: Identifier, body:Exp) extends Value

case class Neu(t:Type,x:Symbol) extends Value