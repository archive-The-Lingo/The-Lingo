/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo.lang

private final object ListUtils {

  final object ValueList {
    def apply(xs: List[Value], tail: Value): Value = xs match {
      case x :: xs => apply(xs, Pair(x, tail))
      case Nil => tail
    }

    def apply(xs: List[Value]): Value = apply(xs, Null())

    def unapply(arg: Value): Option[List[Value]] = arg.reduce_rec().toCore() match {
      case Pair(x, xs) => unapply(xs) match {
        case Some(tail) => Some(x :: tail)
        case None => None
      }
      case Null() => Some(Nil)
      case _ => None
    }
  }

  def consList(xs: Value*): Value = ValueList(xs.toList)
}