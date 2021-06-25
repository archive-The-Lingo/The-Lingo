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

case class NaturalNumber(x: Nat) extends Value {
  def add1: NaturalNumber = NaturalNumber(x+1)
}

case object Absurd extends Type

case class Closure(env: Definitions, x: Identifier, body: Exp) extends Value

case class Neu(t: Type, name: Symbol, id: Nat) extends Value

object Neu {
  private var neuCount: Nat = 0

  def apply(t: Type, name: Symbol): Neu = this.synchronized {
    val result = Neu(t, name, neuCount)
    neuCount = neuCount + 1
    result
  }
}