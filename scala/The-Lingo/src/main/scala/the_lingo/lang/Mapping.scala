/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo.lang

final class Mapping extends WeakHeadNormalForm {
  private val xs: List[(Value, Value)] = List()

  def toCore() = Tagged(Symbols.Mapping, ListUtils.listToValue(xs.map(p => {
    val (p1, p2) = p
    ListUtils.consList(p1, p2)
  })))

  def eval(context: Mapping, stack: DebugStack) = throw new UnsupportedOperationException("TODO")

  def app(xs: List[Value], stack: DebugStack) = throw new UnsupportedOperationException("TODO")

  def equal_reduce_rec(x: Value) = throw new UnsupportedOperationException("TODO")

  def updated(key: Value, value: Value): Mapping = throw new UnsupportedOperationException("TODO")

  def get(key: Value): Option[Value] = throw new UnsupportedOperationException("TODO")
}

final object Mapping {
  val Null = new Mapping()
}
