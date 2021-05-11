/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo.utils

sealed trait Boolean3

final case object Boolean3True extends Boolean3

final case object Boolean3False extends Boolean3

final case object Boolean3Middle extends Boolean3

final object Boolean3 {

  final object Implicits {
    implicit def boolean2boolean3(x: Boolean): Boolean3 = if (x) {
      Boolean3True
    } else {
      Boolean3False
    }
  }

}