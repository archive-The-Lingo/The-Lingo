/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import the.lingo.Value.Implicits._

final case class Mapping private(private val xs: List[(Value, Value)]) extends WHNF {
  override def impl_toCore() = Tagged(Symbols.Tags.Mapping, ListUtils.List(ListUtils.ConsList(xs.map(p => {
    val (p1, p2) = p
    ListUtils.List(p1, p2)
  }))))

  def updated(key: Value, value: Value): Mapping = new Mapping(
    (key, value) :: xs.filterNot(_ match { case (k, v) => k.equal_reduce_rec(key) })
  )

  def get(key: Value): Option[Value] = for {
    (_, v) <- xs.find(_ match {
      case (k, _) => k.equal_reduce_rec(key)
    })} yield v

  def isEmpty: Boolean = xs.isEmpty
}

final object Mapping {
  val Empty = Mapping(Nil)
}

// Attention to immutability
private final object AsMappingCached {

  private final object AsTupleList {

    import ListHelpers._

    def unapply(xs: List[Value]): Option[List[(Value, Value)]] = xs.flatMapOption(_ match {
      case ListUtils.ConsList(List(a, b)) => Some((a, b))
      case _ => None
    })
  }

  private val unapply_v = Value.cached_option_as((arg: WHNF) => arg match {
    case x: Mapping => Some(x)
    case _ => arg.toCore() match {
      case Tagged(AsSym(Symbols.Tags.Mapping), ListUtils.List(ListUtils.ConsList(AsTupleList(xs)))) => Some(Mapping(xs))
      case _ => None
    }
  })

  def unapply(x: Value): Option[Mapping] = unapply_v.apply(x)
}