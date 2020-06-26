/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import org.scalatest.funspec.AnyFunSpec
import Value.Implicits._

final class Tests extends AnyFunSpec {
  describe("equals") {
    it("booleans") {
      assert(Tagged(Sym("陽"), Null()).equal_reduce_rec(ValueBoolean.True))
    }
  }
}
