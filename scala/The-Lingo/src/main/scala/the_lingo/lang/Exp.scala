/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo.lang

sealed trait Exp extends WeakHeadNormalForm {
  def real_eval(context: Mapping): Value

  def eval(context: Mapping) = Value(new Delay({
    this.real_eval(context)
  }, (context, this)))
}

final case class Quote(x: Value) extends Exp {
  def toCore() = throw new UnsupportedOperationException("TODO")

  def real_eval(context: Mapping) = x

  def apply(xs: List[Value]) = throw new UnsupportedOperationException("TODO")
}

final case class Comment(comment: Value, x: Value) extends Exp {
  def toCore() = throw new UnsupportedOperationException("TODO")

  def real_eval(context: Mapping) = x.eval(context)

  def apply(xs: List[Value]) = x.apply(xs)
}

final case class Apply(f: Value, xs: List[Value]) extends Exp {
  def toCore() = throw new UnsupportedOperationException("TODO")

  def real_eval(context: Mapping) = f.eval(context).apply(xs.map((x: Value) => x.eval(context)))

  def apply(xs: List[Value]) = throw new UnsupportedOperationException("TODO")
}

// TODO
