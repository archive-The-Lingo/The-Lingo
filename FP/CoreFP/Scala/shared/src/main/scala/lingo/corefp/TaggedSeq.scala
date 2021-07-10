package lingo.corefp

object TaggedSeq {
  def apply(tag: Value, xs: Value*): Value = Tagged(tag, ValueList(xs.toList))

  def unapplySeq(x: Value): Option[Seq[Value]] = x match {
    case Tagged(tag, ValueList(xs)) => Some(tag :: xs)
    case _ => None
  }
}
