package lingo.corefp

import lingo.corefp.utils.RationalChar

object ValueRationalChar {
  def apply(x: RationalChar): Value = Value.addComponent(x, TaggedSeq(Atoms.Tags.Char, ValueNat(x.toNat)))

  def unapply(x: Value): Option[RationalChar] = Value.getComponentOrAddOption(x, {
    x match {
      case TaggedSeq(Atoms.Tags.Char, ValueNat(n)) => Some(RationalChar(n))
      case _ => None
    }
  })
}
