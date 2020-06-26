/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import the.lingo.Value.Implicits._

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Positional

final case class LangParser(file: String) extends RegexParsers {
  override def skipWhitespace = false

  private def space_regex: Parser[String] = whiteSpace

  private def skipSpace: Parser[String] = space_regex | ""

  private def skipSpace[A](x: Parser[A]): Parser[A] = skipSpace ~ x ^^ {
    case _ ~ x => x
  }

  private def sym_regex: Parser[String] = """(\w|[-？?/])+""".r

  private def sym: Parser[Sym] =
    sym_regex ^^ {
      case x => Sym(Symbol(x))
    }

  private def list: Parser[Value] =
    "(" ~> repsep(value, space_regex) ~ opt(space_regex ~ "." ~ space_regex ~> value) <~ skipSpace(")") ^^ {
      case xs ~ Some(tail) => ListUtils.ConsList(xs, tail)
      case xs ~ None => ListUtils.ConsList(xs)
    }

  private def tagged: Parser[Value] =
    "&(" ~> value ~ space_regex ~ repsep(value, space_regex) ~ opt(space_regex ~ "." ~ space_regex ~> value) <~ skipSpace(")") ^^ {
      case x ~ sp ~ xs ~ Some(tail) => Tagged(x, ListUtils.ConsList(xs, tail))
      case x ~ sp ~ xs ~ None => Tagged(x, ListUtils.ConsList(xs))
    }

  private def id: Parser[Exp] =
    "$" ~> value ^^ {
      Id(_)
    }

  private def applyFunc: Parser[Exp] =
    "[" ~> exp ~ rep(space_regex ~> exp) <~ skipSpace("]") ^^ {
      case f ~ xs => ApplyFunc(f, xs)
    }

  private def applyMacro: Parser[Exp] =
    "{" ~> exp ~ rep(space_regex ~> exp) <~ skipSpace("}") ^^ {
      case f ~ xs => ApplyMacro(f, xs)
    }

  private def quote: Parser[Exp] =
    "'" ~> value ^^ {
      Quote(_)
    }

  private def comment: Parser[Exp] =
    "#(" ~> exp ~ space_regex ~ exp <~ skipSpace(")") ^^ {
      case cmt ~ sp ~ x => Comment(cmt, x)
    }

  private def builtin: Parser[Exp] =
    "!(" ~> sym ~ rep(space_regex ~> exp) <~ skipSpace(")") ^^ {
      case f ~ xs => Builtin(f, xs)
    }

  private def exp: Parser[Exp] = skipSpace(posed(id | applyFunc | applyMacro | quote))

  def parseExpAsOption(x: String): Option[Exp] = parseAll(exp, x) match {
    case Success(result, _) => Some(result)
    case _: NoSuccess => None
  }

  def value: Parser[Value] = skipSpace(sym ^^ {
    Value(_)
  } | list | tagged | exp ^^ {
    Value(_)
  })

  def parseValueAsOption(x: String): Option[Value] = parseAll(value, x) match {
    case Success(result, _) => Some(result)
    case _: NoSuccess => None
  }

  private def pos: Parser[Positional] = positioned(success(new Positional {}))

  private def posed(x: Parser[Exp]): Parser[Exp] = pos ~ x ~ pos ^^ {
    case start ~ x ~ end => Positioned(FilePosition(file, start.pos, end.pos), x)
  }
}
