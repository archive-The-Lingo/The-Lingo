package lingo.corefp

import lingo.corefp.utils.RationalString

object ValueRationalString {
  def apply(x: RationalString): Value = Value.addComponent(x, TaggedSeq(Atoms.Tags.String, todo()))

}
