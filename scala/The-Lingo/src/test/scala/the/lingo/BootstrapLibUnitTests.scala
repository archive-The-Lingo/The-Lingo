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
    lazy val mod = SimpleFileParser(libDir("序列.包")).parseExp().eval() match {
      case AsMappingCached(x) => x
      case x => throw new AssertionError("not a mapping " + x.toString)
    }
    it("parses!") {
      mod
    }

    def testList = ListUtils.List(Id(Id(Id(Id(Id(Sym("甲")))))))

    lazy val mapTestResult = {
      val id = InterpretedClosure(List(Id(Sym("甲"))), None, Mapping.Empty, Id(Sym("甲")))

      val map = mod.get(Sym("內用")).get
      val result = map.app(List(testList, id)).reduce_rec()
      result
    }
    it("map works!") {
      assert(testList.equal_reduce_rec(mapTestResult))
    }
    it("prints!") {
      mapTestResult
      val modStr = mod.toString()
      println(modStr)
      assert(modStr.contains("構造-序列/連結"))
    }
    it("append woeks!") {
      def testList1 = ListUtils.List(Id(Id(Id(Id(Id(Sym("甲")))))), Sym("甲"))

      def testList2 = ListUtils.List(Id(Id(Id(Id(Id(Sym("乙")))))), Sym("乙"))

      def result = ListUtils.List(Id(Id(Id(Id(Id(Sym("甲")))))), Sym("甲"), Id(Id(Id(Id(Id(Sym("乙")))))), Sym("乙"))

      val append = mod.get(Sym("連")).get
      assert(append.app(List(testList1, testList2)).equal_reduce_rec(result))
    }
  }
}
