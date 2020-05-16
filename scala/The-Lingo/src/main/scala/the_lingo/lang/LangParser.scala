/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo.lang

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Positional

final case class LangParser(file: String) extends RegexParsers {
  private def space_regex: Parser[String] = whiteSpace

  private def sym_regex = """(\w|[-？?/])+""".r

  private def sym: Parser[Value] =
    sym_regex ^^ {
      case x => Sym(Symbol(x))
    }

  private def list: Parser[Value] =
    "(" ~> repsep(value, space_regex) ~ opt(space_regex ~ "." ~ space_regex ~> value) <~ ")" ^^ {
      case xs ~ Some(tail) => ListUtils.ValueList(xs, tail)
      case xs ~ None => ListUtils.ValueList(xs)
    }

  private def tagged: Parser[Value] =
    "&(" ~> value ~ space_regex ~ repsep(value, space_regex) ~ opt(space_regex ~ "." ~ space_regex ~> value) <~ ")" ^^ {
      case x ~ sp ~ xs ~ Some(tail) => Tagged(x, ListUtils.ValueList(xs, tail))
      case x ~ sp ~ xs ~ None => Tagged(x, ListUtils.ValueList(xs))
    }

  private def id: Parser[Exp] =
    "$" ~> value ^^ {
      Id(_)
    }

  private def applyFunc: Parser[Exp] =
    "[" ~> exp ~ rep(space_regex ~> exp) <~ "]" ^^ {
      case f ~ xs => ApplyFunc(f, xs)
    }

  private def applyMacro: Parser[Exp] =
    "{" ~> exp ~ rep(space_regex ~> exp) <~ "}" ^^ {
      case f ~ xs => ApplyMacro(f, xs)
    }

  private def quote: Parser[Exp] =
    "'" ~> value ^^ {
      Quote(_)
    }

  private def comment: Parser[Exp] =
    "#(" ~> exp ~ space_regex ~ exp <~ ")" ^^ {
      case cmt ~ sp ~ x => Comment(cmt, x)
    }

  private def builtin: Parser[Exp] =
    "!(" ~> exp ~ rep(space_regex ~> exp) <~ ")" ^^ {
      case f ~ xs => Builtin(f, xs)
    }

  def exp: Parser[Exp] = posed(id | applyFunc | applyMacro | quote)

  def value: Parser[Value] = sym | list | tagged | exp ^^ {
    Value(_)
  }

  private def pos: Parser[Positional] = positioned(success(new Positional {}))

  private def posed(x: Parser[Exp]): Parser[Exp] = pos ~ x ~ pos ^^ {
    case start ~ x ~ end => PositionedExp(DebugStackElement(file, start.pos, end.pos), x)
  }
}
