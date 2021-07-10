package lingo.corefp

import lingo.corefp.utils

object Nat {
  def parse(x: Value): Option[utils.Nat] = Value.getComponentOrAddOption(x, {todo()})
}
