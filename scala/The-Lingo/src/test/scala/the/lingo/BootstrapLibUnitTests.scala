/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import org.scalatest.funspec.AnyFunSpec
import Value.Implicits._

final class BootstrapLibUnitTests extends AnyFunSpec {
  private val libDir = "../../bootstrap-lib/"

  private def libDir(x: String): String = libDir + x

  describe("序列") {
    it("parses!") {
      SimpleFileParser(libDir("序列.包")).parseExp()
    }
    it("map works!") {
      val mod = SimpleFileParser(libDir("序列.包")).parseExp().eval() match {
        case AsMappingCached(x) => x
        case x => throw new AssertionError("not a mapping " + x.toString)
      }
      val id = InterpretedClosure(List(Id(Sym("甲"))), None, Mapping.Empty, Id(Sym("甲")))
      def testList = ListUtils.List(Id(Id(Id(Id(Id(Sym("甲")))))))
      val map = mod.get(Sym("序列-內用")).get
      val result = map.app(List(testList, id)).reduce_rec()
      // TODO
      //result.equal_reduce_rec(testList)
      //println(result.toString())
      //assert(result.equal_reduce_rec(testList))
    }
  }
}
