package lingo.corefp

import fastparse._, NoWhitespace._

final case class SimpleParser(file: String = "") {
  private def whitespace[_: P] = P(CharsWhileIn(" \r\n\t"))

  private def skipWhitepace[_: P] = P(CharsWhileIn(" \r\n\t", 0))

  def value[_: P]: P[Value] = P(atom | list | tagged)

  def exp[_: P]: P[Exp] = P(apply)

  // [\u4e00-\u9fa5] is Chinese chars
  private val atomRegex = "(\\w|[-？?/*:><]|[\u4e00-\u9fa5])+".r

  private def isAtomChar(c: Char) = atomRegex.matches(c.toString)

  def atom[_: P] = P(CharsWhile(isAtomChar).!).map(Atom(_))

  private def values[_: P]: P[List[Value]] = P(skipWhitepace ~ value.rep(sep = whitespace./)).map(xs => xs.toList)

  private def exps[_: P]: P[List[Exp]] = P(skipWhitepace ~ exp.rep(sep = whitespace./)).map(xs => xs.toList)

  def list[_: P]: P[Value] = P("(" ~/ skipWhitepace ~ values ~ skipWhitepace ~ ")").map(ValueList(_))

  def tagged[_: P]: P[Tagged] = P("#(" ~/ skipWhitepace ~ value ~ whitespace ~ values ~ skipWhitepace ~ ")").map(x => Tagged(x._1, ValueList(x._2)))

  def apply[_: P]: P[Exp] = P("[" ~/ skipWhitepace ~ exp ~ whitespace ~ exps ~ skipWhitepace ~ "]").map(x => ApplyFunction(x._1, x._2))
}
