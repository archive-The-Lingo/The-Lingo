/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import org.scalatest.funspec.AnyFunSpec
import Value.Implicits._

final class UnitTests extends AnyFunSpec {
  describe("equals") {
    it("booleans") {
      assert(Tagged(Symbols.Tags.True, Null()).equal_reduce_rec(ValueBoolean.True))
      assert(!Tagged(Symbols.Tags.False, Null()).equal_reduce_rec(ValueBoolean.True))
    }
  }
  describe("parser") {
    it("list") {
      assert(LangParser("file").parseValue(" (    x   y  )")
        .equal_reduce_rec(ValueList(List(Sym("x"), Sym("y")))))
      assert(LangParser("file").parseValue("(x y . z)")
        .equal_reduce_rec(ListUtils.ConsListMaybeWithTail(List(Sym("x"), Sym("y")), Sym("z"))))
      assert(LangParser("file").parseValue(" (    x   y z a   b  . c )   ")
        .equal_reduce_rec(LangParser("file").parseValue("(x y z a b . c)")))
    }
  }
}
