package lingo.corefp

import fastparse._, NoWhitespace._

final case class SimpleParser(file: String = "") {
  private def whitespace[_: P] = P(CharsWhileIn(" \r\n\t"))

  private def skipWhitespace[_: P] = P(CharsWhileIn(" \r\n\t", 0))

  def value[_: P]: P[Value] = P(atom | list | tagged)

  def exp[_: P]: P[Exp] = P(applyFunction | applyMacro | quote | builtin)

  // [\u4e00-\u9fa5] is Chinese chars
  private val atomRegex = "(\\w|[-？?/*:><]|[\u4e00-\u9fa5])+".r

  private def isAtomChar(c: Char) = atomRegex.matches(c.toString)

  def atom[_: P]: P[Atom] = P(CharsWhile(isAtomChar).!).map(Atom(_))

  private def values[_: P]: P[List[Value]] = P(skipWhitespace ~ value.rep(sep = whitespace./)).map(xs => xs.toList)

  private def exps[_: P]: P[List[Exp]] = P(skipWhitespace ~ exp.rep(sep = whitespace./)).map(xs => xs.toList)

  // todo: dot
  def list[_: P]: P[Value] = P("(" ~/ skipWhitespace ~ values ~ skipWhitespace ~ ")").map(ValueList(_))

  def tagged[_: P]: P[Value] = P("#(" ~/ skipWhitespace ~ value ~ whitespace ~ values ~ skipWhitespace ~ ")").map(x => Tagged(x._1, ValueList(x._2)))

  def applyFunction[_: P]: P[Exp] = P("[" ~/ skipWhitespace ~ exp ~ whitespace ~ exps ~ skipWhitespace ~ "]").map(x => ApplyFunction(x._1, x._2))

  def applyMacro[_: P]: P[Exp] = P("{" ~/ skipWhitespace ~ exp ~ whitespace ~ exps ~ skipWhitespace ~ "}").map(x => ApplyMacro(x._1, x._2))

  def quote[_: P]: P[Exp] = P("'" ~/ value).map(Quote)

  private def success[T, _: P](x: T): P[T] = todo()

  private def fail[T, _: P](): P[T] = todo()

  def builtin[_: P] = P("@[" ~/ skipWhitespace ~ atom ~ whitespace ~ exps ~ skipWhitespace ~ "]").flatMap(x => GeneralBuiltin(x._1, x._2) match {
    case GeneralBuiltinExtractor(x) => success(x)
    case _ => fail()
  })
}
