/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import the.lingo.Value.Implicits._

final object SimpleCoreNormalFormPrinter {
  private val sym_regex = SimpleFileParser.sym_regex

  def apply(x: Value): String = x match {
    case ListUtils.ConsList(xs) => s"(${xs.map(apply).mkString(" ")})"
    case ListUtils.ConsListMaybeWithTail(xs, tail) => s"(${xs.map(apply).mkString(" ")} . ${apply(tail)})"
    case _ => x.reduce_rec_toCore() match {
      case Null | Pair(_, _) => throw new IllegalStateException()
      case Sym(x) => {
        val s = x.toString()
        assert(sym_regex.matches(s))
        assert(!s.contains(" "))
        s
      }
      case Tagged(x, xs) => "&" + apply(Pair(x, xs))
      case ValueException(x, xs) => "^" + apply(Pair(x, xs))
      case ValueNat(x) => x.toString()
    }
  }
}
