package pie

import scala.collection.immutable.HashMap

type Identifier = Symbol

type Nat = Int

type Definitions = HashMap[Identifier, (Type, Value)]

val globalDefinitions: Definitions = HashMap((Symbol("Absurd"), (U(1), Absurd)))

sealed trait Exp {

}

case class The(valueType: Exp, value: Exp) extends Exp

case class Lambda(argument: Identifier, body: Exp) extends Exp

case class Variable(x: Identifier) extends Exp

case class ElimNat(target: Exp, motive: Exp, base: Exp, step: Exp) extends Exp

case class ElimAbsurd(target: Exp, motive: Exp) extends Exp
