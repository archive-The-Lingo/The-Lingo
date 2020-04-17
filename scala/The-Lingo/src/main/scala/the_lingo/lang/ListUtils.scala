/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo.lang

private final object ListUtils {
  def listToValue(xs: List[Value]): Value = listToValue(xs, Null())

  def listToValue(xs: List[Value], tail: Value): Value = xs match {
    case x :: xs => listToValue(xs, Pair(x, tail))
    case Nil => tail
  }

  def consList(xs: Value*): Value = listToValue(xs.toList)
}