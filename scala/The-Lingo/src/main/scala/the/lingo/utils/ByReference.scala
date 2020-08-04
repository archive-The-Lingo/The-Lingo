/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo.utils

final class ByReference[A](val x: A) {
  def get: A = x
}

final object ByReference {
  def apply[A](x: A): ByReference[A] = new ByReference(x)

  final object Implicits {
    implicit def packByReference[A](x: A): ByReference[A] = ByReference(x)

    implicit def unpackByReference[A](x: ByReference[A]): A = x.get
  }

}