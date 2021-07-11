package lingo.corefp

import fastparse._, NoWhitespace._

final case class SimpleParser(file: String = "") {
  private def whitespace[_: P] = P(CharsWhileIn(" \r\n\t"))

  private def skipWhitepace[_: P] = P(CharsWhileIn(" \r\n\t", 0))

  def value[_: P]: P[Value] = P(atom | list)

  // [\u4e00-\u9fa5] is Chinese chars
  private val atomRegex = "(\\w|[-？?/*:><]|[\u4e00-\u9fa5])+".r

  private def isAtomChar(c: Char) = atomRegex.matches(c.toString)

  def atom[_: P] = P(CharsWhile(isAtomChar).!).map(Atom(_))

  def list[_: P]: P[Value] = P("(" ~/ skipWhitepace ~/ (value ~ skipWhitepace).rep(sep = whitespace./) ~ skipWhitepace ~ ")").map(xs => ValueList(xs.toList))
}
