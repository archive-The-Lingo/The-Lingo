package lingo.corefp

import fastparse._
import NoWhitespace._
import lingo.corefp.utils.Nat

final case class SimpleParserFailure(expected: String, index: Nat, aggregateMsg: String)

final case class SimpleParser(file: String = "") {
  private def whitespace[_: P] = P(CharsWhileIn(" \r\n\t"))

  private def skipWhitespace[_: P] = P(CharsWhileIn(" \r\n\t", 0))

  def value[_: P]: P[Value] = P(valueExp | atom | list | tagged)

  private def valueEnd[_: P]: P[Value] = P(value ~ End)

  def parseValue(x: String): Either[SimpleParserFailure, Value] = parse(x, valueEnd(_)) match {
    case Parsed.Success(x, _) => Right(x)
    case Parsed.Failure(expected, failIndex, extra) => Left(SimpleParserFailure(expected, Nat(failIndex), extra.trace().longAggregateMsg))
    case _ => throw new IllegalStateException() // suppress warning
  }

  private def valueExp[_: P] = P(exp).map(ValueExp(_))

  def exp[_: P]: P[Exp] = P((parserLocation ~ (applyFunction | applyMacro | quote | builtin | variable | lambda | rec | comment)).map(x => Located(x._1, x._2)) | located)

  // [\u4e00-\u9fa5] is Chinese chars
  private val atomRegex = "(\\w|[-？?/*:><]|[\u4e00-\u9fa5])+".r

  private def isAtomChar(c: Char) = atomRegex.matches(c.toString)

  def atom[_: P]: P[Atom] = P(CharsWhile(isAtomChar).!).map(Atom(_))

  private def values[_: P]: P[List[Value]] = P(skipWhitespace ~ value.rep(sep = whitespace./)).map(xs => xs.toList)

  private def exps[_: P]: P[List[Exp]] = P(skipWhitespace ~ exp.rep(sep = whitespace./)).map(xs => xs.toList)

  // todo: dot
  def list[_: P]: P[Value] = P("(" ~/ skipWhitespace ~ values ~ skipWhitespace ~ ")").map(ValueList(_))

  def tagged[_: P]: P[Tagged] = P("#(" ~/ skipWhitespace ~ value ~ whitespace ~ values ~ skipWhitespace ~ ")").map(x => Tagged(x._1, ValueList(x._2)))

  def applyFunction[_: P]: P[ApplyFunction] = P("[" ~/ skipWhitespace ~ exp ~ whitespace ~ exps ~ skipWhitespace ~ "]").map(x => ApplyFunction(x._1, x._2))

  def applyMacro[_: P]: P[ApplyMacro] = P("{" ~/ skipWhitespace ~ exp ~ whitespace ~ exps ~ skipWhitespace ~ "}").map(x => ApplyMacro(x._1, x._2))

  def quote[_: P]: P[Quote] = P("'" ~/ value).map(Quote)

  def builtin[_: P]: P[Builtin] = P("@[" ~/ skipWhitespace ~ atom ~ whitespace ~ exps ~ skipWhitespace ~ "]").flatMap(x => GeneralBuiltin(x._1, x._2) match {
    case GeneralBuiltinExtractor(x) => Pass.map(_ => x)
    case _ => Fail.opaque("Illegal Builtin")
  })

  def variable[_: P]: P[Var] = P("$" ~/ value).map(Var)

  def lambda[_: P]: P[Function] = P("={" ~/ skipWhitespace ~ value ~ whitespace ~ exp ~ "}").flatMap({
    case (ValueArgs(args), body) => Pass.map(_ => Function(args, body))
    case _ => Fail.opaque("Illegal arguments pattern")
  })

  def rec[_: P]: P[Recursive] = P("*{" ~/ skipWhitespace ~ variable ~ whitespace ~ exp ~ "}").map(x => Recursive(x._1, x._2))

  def comment[_: P]: P[Commented] = P("%{" ~/ skipWhitespace ~ value ~ whitespace ~ exp ~ "}").map(x => Commented(x._1, x._2))

  def located[_: P]: P[Located] = P("~{" ~/ skipWhitespace ~ value ~ whitespace ~ exp ~ "}").flatMap({
    case (ValueLocation(loc), expr) => Pass.map(_ => Located(loc, expr))
    case _ => Fail.opaque("Illegal Location")
  })

  def parserLocation[_: P]: P[Location] = P(Index).map(x => UNIXFileLocation(file, Nat(x), None))

  // TODO
  def parseIncludeValue[_: P]: P[Value] = P("`{" ~/ ??? ~ "}").map(???)
}
