/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo.utils

final class OnewayWriteFlag {
  private val flag = new MutableBox(false)

  def set: Unit = {
    flag.update(true)
  }

  def get: Boolean = flag.get

  def or(x: Boolean): Unit = {
    if (x) {
      this.set
    }
  }
}
