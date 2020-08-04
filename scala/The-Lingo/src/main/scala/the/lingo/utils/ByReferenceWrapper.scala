/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo.utils

final class ByReferenceWrapper[A](val x: A) {
  def get: A = x

  override def hashCode(): Int = System.identityHashCode(x)
}

final object ByReferenceWrapper {
  def apply[A](x: A): ByReferenceWrapper[A] = new ByReferenceWrapper(x)

  final object Implicits {
    implicit def packByReferenceWrapper[A](x: A): ByReferenceWrapper[A] = ByReferenceWrapper(x)

    implicit def unpackByReferenceWrapper[A](x: ByReferenceWrapper[A]): A = x.get
  }

}