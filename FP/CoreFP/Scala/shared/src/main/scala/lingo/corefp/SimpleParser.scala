package lingo.corefp

import fastparse._

object SimpleParser {
  // [\u4e00-\u9fa5] is Chinese chars
  val atomRegex = """(\w|[-？?/*:><]|[\u4e00-\u9fa5])+""".r

  def atom[_: P] = P(CharsWhile(c => atomRegex.matches(c.toString)).!)
}
