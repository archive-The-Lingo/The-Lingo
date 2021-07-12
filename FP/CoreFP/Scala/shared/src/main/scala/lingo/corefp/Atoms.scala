/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package lingo.corefp

object Atoms {
  val Builtin: Atom = Atom("內建")
  val Func: Atom = Atom("函式")

  object Tags {
    val BinaryNat: Atom = Atom("自然數-二進位")
    val False: Atom = Atom("陰")
    val True: Atom = Atom("陽")
    val Char: Atom = Atom("字符")
    val String: Atom = Atom("字串")
    val Mapping: Atom = Atom("映射")
  }

  object Builtins {
    val IsAtom: Atom = Atom("符號？")
    val IsEmptyList: Atom = Atom("序列/空？")
    val IsNonEmptyList: Atom = Atom("序列/非空？")
    val IsTagged: Atom = Atom("具型？")
    val IsException: Atom = Atom("異常？")
    val IsResource: Atom = Atom("資源？")
    val IntroNonEmptyList: Atom = Atom("構造-序列/非空")
    val ElimNonEmptyListHead: Atom = Atom("序列/非空.首")
    val ElimNonEmptyListTail: Atom = Atom("序列/非空.尾")
    val IntroTagged: Atom = Atom("構造-具型")
    val ElimTaggedTag: Atom = Atom("具型.型")
    val ElimTaggedData: Atom = Atom("具型.值")
    val IntroException: Atom = Atom("構造-異常")
    val ElimExceptionTag: Atom = Atom("異常.型")
    val ElimExceptionData: Atom = Atom("異常.值")
    val ElimResourceTag: Atom = Atom("資源.型")
    val ElimResourceData: Atom = Atom("資源.值")
    val ElimBoolean: Atom = Atom("若")
    val Equal: Atom = Atom("等？")
  }

  object ExceptionReasons {
    val IllegalExp: Atom = Atom("非法式")
    val NoDefinition: Atom = Atom("無定義")
    val TypeMismatch: Atom = Atom("型不匹配")
  }
}

/*
final object Atoms {
  val Core: Atom = Atom("核")
  val Id: Atom = Atom("識別子")
  val Exp: Atom = Atom("式")
  val ApplyFunc: Atom = Atom("用-函式")
  val Quote: Atom = Atom("引用")
  val ApplyMacro: Atom = Atom("用-構式子")
  val Comment: Atom = Atom("注釋")
  val Exception: Atom = Atom("異常")
  val Builtin: Atom = Atom("內建")
  val Nat: Atom = Atom("自然數")
  val Positioned: Atom = Atom("具座標")
  val Func: Atom = Atom("函式")

  final object Builtins {
    //val NatZero: Atom = Atom("自然數/零")
    //val NatSucc: Atom = Atom("自然數/增一")
    val IsPair: Atom = Atom("序列/連結？")
    val ConsList: Atom = Atom("構造-序列*")
    val ConsPair: Atom = Atom("構造-序列/連結")
    val ElimPair: Atom = Atom("解構-序列/連結")
    val IsTagged: Atom = Atom("具標籤？")
    val ConsTagged: Atom = Atom("構造-具標籤")
    val ElimTagged: Atom = Atom("解構-具標籤")
    val IsException: Atom = Atom("異常？")
    val ConsException: Atom = Atom("構造-異常")
    val ElimException: Atom = Atom("解構-異常")
    val IsNull: Atom = Atom("序列/空？")
    val IsSymbol: Atom = Atom("符號？")
    val SymbolToString: Atom = Atom("符號→字串")
    val StringToSymbol: Atom = Atom("字串→符號")
    val Rec: Atom = Atom("遞歸")
    val NatToBinary: Atom = Atom("自然數→二進位")
    val BinaryToNat: Atom = Atom("二進位→自然數")
    val Eval: Atom = Atom("解算")
    val ElimBoolean: Atom = Atom("解構-陰陽")
    val AppendMapping: Atom = Atom("連-映射")
  }

  final object Tags {
    val UNIXFilePosition: Atom = Atom("座標/UNIX文件")
    val NamedPosition: Atom = Atom("座標/識別子")
    val False: Atom = Atom("陰")
    val True: Atom = Atom("陽")
    val Char: Atom = Atom("字符")
    val String: Atom = Atom("字串")
    val Mapping: Atom = Atom("映射")
    val Macro: Atom = Atom("構式子")
  }

  final object CoreExceptions {
    val IllegalExp: Atom = Atom("非法式")
    val NoDefinition: Atom = Atom("無定義")
    val ArgsMismatch: Atom = Atom("參數不匹配")
    val TypeMismatch_Func: Atom = Atom("標籤不匹配/函式")
    val TypeMismatch_Exp: Atom = Atom("標籤不匹配/式")
    val TypeMismatch_Pair: Atom = Atom("標籤不匹配/序對")
    val TypeMismatch_Tagged: Atom = Atom("標籤不匹配/具標籤")
    val TypeMismatch_Exception: Atom = Atom("標籤不匹配/異常")
    val TypeMismatch_Nat: Atom = Atom("標籤不匹配/自然數")
    val TypeMismatch_Binary: Atom = Atom("標籤不匹配/二進位")
    val TypeMismatch_Symbol: Atom = Atom("標籤不匹配/符號")
    val TypeMismatch_String: Atom = Atom("標籤不匹配/字串")
    val TypeMismatch_Mapping: Atom = Atom("標籤不匹配/映射")
    val TypeMismatch_List: Atom = Atom("標籤不匹配/序列")
    val TypeMismatch_Macro: Atom = Atom("標籤不匹配/構式子")
    val TypeMismatch_Boolean: Atom = Atom("標籤不匹配/陰陽")
  }

}*/