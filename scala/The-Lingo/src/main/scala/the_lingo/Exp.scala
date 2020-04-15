/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo

sealed trait Exp extends UnboxedValue {
  def readback(): (Mapping, Exp) = (new Mapping(), Quote(Value(this)))
}

final case class Quote(x:Value) extends Exp {
  def reduce() = throw new UnsupportedOperationException("TODO")

  def eval(context: Mapping) = x

  def apply(xs: List[Value]) = throw new UnsupportedOperationException("TODO")
}

// TODO