package lingo.corefp

trait Value {
  def core: CoreValue

  override def hashCode(): Int = core.hashCode()
}
