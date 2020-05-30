/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

final case class Mapping private(private val xs: List[(Value, Value)]) extends WHNF {
  override def toCore() = Tagged(Symbols.Tags.Mapping, ListUtils.ConsList(xs.map(p => {
    val (p1, p2) = p
    ListUtils.list(p1, p2)
  })))

  def updated(key: Value, value: Value): Mapping = new Mapping(
    (key, value) :: xs.filterNot(_ match { case (k, v) => k.equal_reduce_rec(key) })
  )

  def get(key: Value): Option[Value] = for {
    (k, v) <- xs.find(_ match {
      case (k, v) => k.equal_reduce_rec(key)
    })} yield v
}

final object Mapping {
  val Null = new Mapping(List())
}
