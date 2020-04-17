/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo.lang

import scala.util.parsing.combinator.RegexParsers

final object ExpParser extends RegexParsers {
  def id: Parser[Exp] =
    """(\w|[-？?/])+""".r ^^ {
      case x => Id(Value(Sym(Symbol(x))))
    }

  def space: Parser[String] = whiteSpace

  def applyFunc: Parser[Exp] =
    "[" ~> exp ~ rep1sep(exp, space) <~ "]" ^^ {
      case f ~ xs => ApplyFunc(f, xs)
    }

  def applyMacro: Parser[Exp] =
    "{" ~> exp ~ rep1sep(exp, space) <~ "}" ^^ {
      case f ~ xs => ApplyMacro(f, xs)
    }

  // TODO
  def exp: Parser[Exp] = id | applyFunc | applyMacro
}
