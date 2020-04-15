package the_lingo

trait Value {

}

sealed trait WeakHeadNormalForm extends Value

final case class Null() extends WeakHeadNormalForm

final case class Symbol(x: String) extends WeakHeadNormalForm

final case class Pair(x: Value, y: Value) extends WeakHeadNormalForm

final case class Tagged(tag: Value, xs: Value) extends WeakHeadNormalForm
