package lingo.corefp.utils

import scala.collection.mutable.ListBuffer

final case class RationalString(xs: List[RationalChar]) {
  def toList: List[RationalChar] = xs

  def valString: String = xs.map(_.valString).mkString
}

object RationalString {
  def apply(x: String): RationalString = {
    // https://web.archive.org/web/20210710054438/https://stackoverflow.com/questions/1527856/how-can-i-iterate-through-the-unicode-codepoints-of-a-java-string/1527891
    val xs = ListBuffer[RationalChar]()
    var offest = 0
    while (offest < x.length) {
      val codePoint = x.codePointAt(offest)

      xs += RationalChar(codePoint)

      offest += Character.charCount(codePoint)
    }
    new RationalString(xs.toList)
  }
}