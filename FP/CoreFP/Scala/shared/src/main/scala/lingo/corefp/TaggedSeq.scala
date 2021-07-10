package lingo.corefp

object TaggedSeq {
  def unapplySeq(x: Value): Option[Seq[Value]] = x match {
    case Tagged(tag, ValueList(xs)) => Some(tag :: xs)
    case _ => None
  }
}
