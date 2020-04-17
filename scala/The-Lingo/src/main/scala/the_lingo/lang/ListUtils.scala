/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo.lang

private final object ListUtils {
  def seqToValue(xs: Seq[Value]): Value = xs match {
    case x :: xs => Value(Pair(x, seqToValue(xs)))
    case Nil => Value(Null())
  }

  def consList(xs: Value*): Value = seqToValue(xs)
}