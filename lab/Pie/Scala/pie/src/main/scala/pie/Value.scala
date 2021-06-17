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

case class NaturalNumber(x: Nat) extends Type {
  
}

case object Absurd extends Type {

}