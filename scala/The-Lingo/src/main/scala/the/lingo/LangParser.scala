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

  private val space_regex: Parser[String] = whiteSpace

  private val skipSpace: Parser[String] = space_regex | ""

  private def skipSpace[A](x: Parser[A]): Parser[A] = skipSpace ~ x ^^ {
    case _ ~ x => x
  }

  private def skipEndSpace[A](x: Parser[A]): Parser[A] = x ~ skipSpace ^^ {
    case x ~ _ => x
  }

  //private def skipBeginAndEndSpace[A](x: Parser[A]): Parser[A] = skipSpace(skipEndSpace(x))

  // same as SimpleCoreNormalFormPrinter's
  private val sym_regex: Parser[String] = """(\w|[-？?/])+""".r

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

  private def exception: Parser[Value] =
    "^(" ~> value ~ space_regex ~ repsep(value, space_regex) ~ opt(space_regex ~ "." ~ space_regex ~> value) <~ skipSpace(")") ^^ {
      case x ~ sp ~ xs ~ Some(tail) => ValueException(x, ListUtils.ConsList(xs, tail))
      case x ~ sp ~ xs ~ None => ValueException(x, ListUtils.ConsList(xs))
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

  private val exp: Parser[Exp] = skipSpace(posed(id | applyFunc | applyMacro | quote | comment | builtin))

  private val topExp = skipEndSpace(exp)

  def parseExpAsOption(x: String): Option[Exp] = parseAll(topExp, x) match {
    case Success(result, _) => Some(result)
    case _: NoSuccess => None
  }

  def parseExpAsEither(x: String): Either[NoSuccess, Exp] = parseAll(topExp, x) match {
    case Success(result, _) => Right(result)
    case x: NoSuccess => Left(x)
  }

  def parseExp(x: String): Exp = parseAll(topExp, x) match {
    case Success(result, _) => result
    case x: NoSuccess => throw new RuntimeException(x.msg)
  }

  private val value: Parser[Value] = skipSpace(sym ^^ {
    Value(_)
  } | list | tagged | exception | exp ^^ {
    Value(_)
  })

  private val topValue = skipEndSpace(value)

  def parseValueAsOption(x: String): Option[Value] = parseAll(topValue, x) match {
    case Success(result, _) => Some(result)
    case _: NoSuccess => None
  }

  def parseValueAsEither(x: String): Either[NoSuccess, Value] = parseAll(topValue, x) match {
    case Success(result, _) => Right(result)
    case x: NoSuccess => Left(x)
  }

  def parseValue(x: String): Value = parseAll(topValue, x) match {
    case Success(result, _) => result
    case x: NoSuccess => throw new RuntimeException(x.msg)
  }

  private def pos: Parser[Positional] = positioned(success(new Positional {}))

  private def posed(x: Parser[Exp]): Parser[Exp] = pos ~ x ~ pos ^^ {
    case start ~ x ~ end => Positioned(FilePosition(file, start.pos, end.pos), x)
  }
}
