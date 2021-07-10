package lingo.corefp

object ValueBoolean extends ValueT[Boolean] {
  val True: Value = Tagged(Atoms.Tags.True, EmptyList)
  val False: Value = Tagged(Atoms.Tags.False, EmptyList)

  def apply(x: Boolean): Value = if (x) {
    True
  } else {
    False
  }

  def unapply(x: Value): Option[Boolean] = Value.getComponentOrAddOption(x, {
    x match {
      case Tagged(Atoms.Tags.True, EmptyList) => Some(true)
      case Tagged(Atoms.Tags.False, EmptyList) => Some(false)
      case _ => None
    }
  })
}
