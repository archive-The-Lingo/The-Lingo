package lingo.corefp

object ValueBoolean extends ValueT[Boolean] {
  val True: Value = TaggedSeq(Atoms.Tags.True)
  val False: Value = TaggedSeq(Atoms.Tags.False)

  def apply(x: Boolean): Value = if (x) {
    True
  } else {
    False
  }

  def unapply(x: Value): Option[Boolean] = Value.getComponentOrAddOption(x, {
    x match {
      case TaggedSeq(Atoms.Tags.True) => Some(true)
      case TaggedSeq(Atoms.Tags.False) => Some(false)
      case _ => None
    }
  })
}
