/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

import org.scalacheck.Properties
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import Value.Implicits._

final object PropTests extends Properties("the lingo") {
  property("parser") = forAll(Gen.identifier, Gen.identifier) { (idx, idy) =>
    LangParser("file").parseValue(s" (    $idx   $idy  )")
      .equal_reduce_rec(ValueList(List(Sym(idx), Sym(idy))))
  }
}
