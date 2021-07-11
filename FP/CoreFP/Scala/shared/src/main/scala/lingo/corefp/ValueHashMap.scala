package lingo.corefp

import scala.collection.immutable.HashMap

object ValueHashMap extends CachedValueT[HashMap[Value, Value]] {
  type Type = HashMap[Value, Value]
  private val ValueListListPair = ValueListT(ValueListPair)
  override val helper = Helper()

  override def internal_apply(x: HashMap[Value, Value]): Value = TaggedSeq(Atoms.Tags.Mapping, ValueListListPair(x.toList))

  override def internal_unapply(x: Value): Option[HashMap[Value, Value]] = x match {
    case TaggedSeq(Atoms.Tags.Mapping, ValueListListPair(xs)) => Some(HashMap.from(xs))
    case _ => None
  }
}
