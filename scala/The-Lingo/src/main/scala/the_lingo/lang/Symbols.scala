/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo.lang

final object Symbols {
  val Exp: Sym = Sym('式)
  val Id: Sym = Sym('標識符)
  val ApplyFunc: Sym = Sym(Symbol("用-函式"))
  val Macro: Sym = Sym('構式子)
  val Quote: Sym = Sym('引)
  val Func: Sym = Sym('函式)
  val ApplyMacro: Sym = Sym(Symbol("用-構式子"))
  val Comment: Sym = Sym('注釋)
  val Exception: Sym = Sym('異常)
  val Eval: Sym = Sym('解算)
  val Mapping: Sym = Sym('映射)
  val Builtin: Sym = Sym('內建)
  val False: Sym = Sym('陰)
  val True: Sym = Sym('陽)
  val Char: Sym = Sym('字符)
  val String: Sym = Sym('字串)
  val NatZero: Sym = Sym("自然數/零")
  val NatSucc: Sym = Sym("自然數/加一")
  val SymbolToString: Sym = Sym("符號→字串")
  val StringToSymbol: Sym = Sym("字串→符號")
  val ConsPair: Sym = Sym("構造-列表/序對")
  val ElimPair: Sym = Sym("解構-列表/序對")
  val IsPair: Sym = Sym("列表/序對？")
  val ConsTagged: Sym = Sym("構造-具型別體")
  val ElimTagged: Sym = Sym("解構-具型別體")
  val IsNull: Sym = Sym("列表/空？")
  val IsSymbol: Sym = Sym("符號？")
  val Rec: Sym = Sym('遞歸)
}