/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import java.io.File

import the.lingo.Value.Implicits._
import the.lingo.utils.Nat

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Positional

private final object SimpleFileParser {
  // [\u4e00-\u9fa5] is Chinese chars
  val sym_regex = """(\w|[-？?/*:]|[\u4e00-\u9fa5])+""".r
}

final case class SimpleFileParser(file: String) extends RegexParsers {
  override def skipWhitespace = false

  private val spaceRegex: Parser[String] = whiteSpace

  private val skipSpace: Parser[String] = spaceRegex | ""

  private def skipSpace[A](x: Parser[A]): Parser[A] = skipSpace ~ x ^^ {
    case _ ~ x => x
  }

  private def skipEndSpace[A](x: Parser[A]): Parser[A] = x ~ skipSpace ^^ {
    case x ~ _ => x
  }

  //private def skipBeginAndEndSpace[A](x: Parser[A]): Parser[A] = skipSpace(skipEndSpace(x))

  private lazy val dir = new File(file).getParentFile()

  private def includeValue: Parser[Value] = "~|" ~> """[^|]+""".r <~ "|~" ^^ {
    case fileToIncludeRaw => {
      val fileToInclude = new File(dir, fileToIncludeRaw).getCanonicalPath()
      // TODO: fix Exception and close the file
      SimpleFileParser(fileToInclude).parseValue(Source.fromFile(fileToInclude).getLines.mkString)
    }
  }

  private def nat: Parser[Value] =
    """\d+""".r ^^ {
      case x => ValueNat(Nat(x))
    }

  private val symRegex: Parser[String] = SimpleFileParser.sym_regex

  private def sym: Parser[Sym] =
    symRegex ^^ {
      case x => Sym(Symbol(x))
    }

  private def list: Parser[Value] =
    "(" ~> repsep(value, spaceRegex) ~ opt(spaceRegex ~ "." ~ spaceRegex ~> value) <~ skipSpace(")") ^^ {
      case xs ~ Some(tail) => ListUtils.ConsListMaybeWithTail(xs, tail)
      case xs ~ None => ListUtils.ConsList(xs)
    }

  private def tagged: Parser[Value] =
    "&(" ~> value ~ spaceRegex ~ repsep(value, spaceRegex) ~ opt(spaceRegex ~ "." ~ spaceRegex ~> value) <~ skipSpace(")") ^^ {
      case x ~ sp ~ xs ~ Some(tail) => Tagged(x, ListUtils.ConsListMaybeWithTail(xs, tail))
      case x ~ sp ~ xs ~ None => Tagged(x, ListUtils.ConsList(xs))
    }

  private def exception: Parser[Value] =
    "^(" ~> value ~ spaceRegex ~ repsep(value, spaceRegex) ~ opt(spaceRegex ~ "." ~ spaceRegex ~> value) <~ skipSpace(")") ^^ {
      case x ~ sp ~ xs ~ Some(tail) => ValueException(x, ListUtils.ConsListMaybeWithTail(xs, tail))
      case x ~ sp ~ xs ~ None => ValueException(x, ListUtils.ConsList(xs))
    }

  private def id: Parser[Exp] =
    "$" ~> value ^^ {
      Id(_)
    }

  private def applyFunc: Parser[Exp] =
    "[" ~> exp ~ rep(spaceRegex ~> exp) <~ skipSpace("]") ^^ {
      case f ~ xs => ApplyFunc(f, xs)
    }

  private def applyMacro: Parser[Exp] =
    "{" ~> exp ~ rep(spaceRegex ~> exp) <~ skipSpace("}") ^^ {
      case f ~ xs => ApplyMacro(f, xs)
    }

  private def quote: Parser[Exp] =
    "'" ~> value ^^ {
      Quote(_)
    }

  private def comment: Parser[Exp] =
    "#(" ~> exp ~ spaceRegex ~ exp <~ skipSpace(")") ^^ {
      case cmt ~ sp ~ x => Comment(cmt, x)
    }

  private def builtin: Parser[Exp] =
    "!(" ~> sym ~ rep(spaceRegex ~> value) <~ skipSpace(")") ^^ {
      case f ~ xs => Builtin(f, xs)
    }

  private val exp: Parser[Exp] =
    skipSpace(posed(id | applyFunc | applyMacro | quote | comment | builtin))

  private val topExp = skipEndSpace(exp)

  def parseExpAsOption(x: String = Source.fromFile(file).mkString): Option[Exp] = parseAll(topExp, x) match {
    case Success(result, _) => Some(result)
    case _: NoSuccess => None
  }

  def parseExpAsEither(x: String = Source.fromFile(file).mkString): Either[NoSuccess, Exp] = parseAll(topExp, x) match {
    case Success(result, _) => Right(result)
    case x: NoSuccess => Left(x)
  }

  def parseExp(x: String = Source.fromFile(file).mkString): Exp = parseAll(topExp, x) match {
    case Success(result, _) => result
    case x: NoSuccess => throw new RuntimeException(x.toString)
  }

  private implicit def packParser[A <: MayNotWHNF](x: Parser[A]): Parser[Value] = x ^^ {
    Value(_)
  }

  private val value: Parser[Value] =
    skipSpace(sym | list | tagged | exception | nat | includeValue | exp)

  private val topValue = skipEndSpace(value)

  def parseValueAsOption(x: String = Source.fromFile(file).mkString): Option[Value] = parseAll(topValue, x) match {
    case Success(result, _) => Some(result)
    case _: NoSuccess => None
  }

  def parseValueAsEither(x: String = Source.fromFile(file).mkString): Either[NoSuccess, Value] = parseAll(topValue, x) match {
    case Success(result, _) => Right(result)
    case x: NoSuccess => Left(x)
  }

  def parseValue(x: String = Source.fromFile(file).mkString): Value = parseAll(topValue, x) match {
    case Success(result, _) => result
    case x: NoSuccess => throw new RuntimeException(x.toString)
  }

  private def pos: Parser[Positional] = positioned(success(new Positional {}))

  private def posed(x: Parser[Exp]): Parser[Exp] = pos ~ x ~ pos ^^ {
    case start ~ x ~ end => Positioned(FilePosition(file, start.pos, end.pos), x)
  }
}
