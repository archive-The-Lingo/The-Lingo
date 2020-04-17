/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo.lang

final object Symbols {
  val Exp: Value = Value(Sym('式))
  val Id: Value = Value(Sym('標識符))
  val ApplyFunc: Value = Value(Sym(Symbol("用-函式")))
  val Macro: Value = Value(Sym('構式子))
  val Quote: Value = Value(Sym('引))
  val Func: Value = Value(Sym('函式))
  val ApplyMacro: Value = Value(Sym(Symbol("用-構式子")))
  val Comment: Value = Value(Sym('注釋))
  val Exception: Value = Value(Sym('異常))
  val Eval: Value = Value(Sym('解算))
  val Mapping: Value = Value(Sym('映射))
  val Builtin: Value = Value(Sym('內建))
  val False: Value = Value(Sym('陰))
  val True: Value = Value(Sym('陽))
  val Char: Value = Value(Sym('字符))
  val String: Value = Value(Sym('字串))
  val NatZero: Value = Value(Sym(Symbol("自然數/零")))
  val NatSucc: Value = Value(Sym(Symbol("自然數/加一")))
  val SymbolToString: Value = Value(Sym(Symbol("符號→字串")))
  val StringToSymbol: Value = Value(Sym(Symbol("字串→符號")))
  val ConsPair: Value = Value(Sym(Symbol("構造-列表/序對")))
  val ElimPair: Value = Value(Sym(Symbol("解構-列表/序對")))
  val IsPair: Value = Value(Sym(Symbol("列表/序對？")))
  val ConsTagged: Value = Value(Sym(Symbol("構造-具型別體")))
  val ElimTagged: Value = Value(Sym(Symbol("解構-具型別體")))
  val IsNull: Value = Value(Sym(Symbol("列表/空？")))
  val IsSymbol: Value = Value(Sym(Symbol("符號？")))
  val Rec: Value = Value(Sym('遞歸))
}