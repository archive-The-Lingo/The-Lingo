/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo.lang

final class Mapping extends WeakHeadNormalForm {
  private val xs: List[(Value, Value)] = List()

  def toCore() = throw new UnsupportedOperationException("TODO")

  def eval(context: Mapping) = throw new UnsupportedOperationException("TODO")

  def apply(xs: List[Value]) = throw new UnsupportedOperationException("TODO")
}

object Mapping {
  val Null = new Mapping()
}
