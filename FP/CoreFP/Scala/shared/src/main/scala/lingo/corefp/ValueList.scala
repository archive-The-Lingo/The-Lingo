package lingo.corefp

object ValueList {
  def apply(xs: List[Value]): Value = xs match {
    case Nil => EmptyList
    case head :: tail => NonEmptyList(head, apply(tail))
  }

  def unapply(x: Value): Option[List[Value]] = Value.getComponentOrAddOption(x, {
    x match {
      case EmptyList => Some(Nil)
      case NonEmptyList(head, tail) => unapply(tail).map(head :: _)
      case _ => None
    }
  })
}
