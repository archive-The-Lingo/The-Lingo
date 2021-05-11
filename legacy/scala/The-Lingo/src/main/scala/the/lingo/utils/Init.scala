/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo.utils

final class Init[T] {
  private var storage: Option[T] = None

  def set(x: T): Boolean =
    if (storage.isEmpty) {
      storage = Some(x)
      true
    } else {
      false
    }

  def get: Option[T] = storage
}
