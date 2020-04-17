/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the_lingo.lang

final object Symbols {
  val Exp: Value = Value(Symbol("式"))
  val Id: Value = Value(Symbol("標識符"))
  val ApplyFunc: Value = Value(Symbol("用-函式"))
  val Macro: Value = Value(Symbol("構式子"))
  val Quote: Value = Value(Symbol("引"))
  val Func: Value = Value(Symbol("函式"))
  val ApplyMacro: Value = Value(Symbol("用-構式子"))
  val Comment: Value = Value(Symbol("注釋"))
  val Exception: Value = Value(Symbol("異常"))
  val Eval: Value = Value(Symbol("解算"))
  val Mapping: Value = Value(Symbol("映射"))
  val Builtin: Value = Value(Symbol("內建"))
  val False: Value = Value(Symbol("陰"))
  val True: Value = Value(Symbol("陽"))
  val Char: Value = Value(Symbol("字符"))
  val String: Value = Value(Symbol("字串"))
  val NatZero: Value = Value(Symbol("自然數/零"))
  val NatSucc: Value = Value(Symbol("自然數/加一"))
  val SymbolToString: Value = Value(Symbol("符號→字串"))
  val StringToSymbol: Value = Value(Symbol("字串→符號"))
  val ConsPair: Value = Value(Symbol("構造-列表/序對"))
  val ElimPair: Value = Value(Symbol("解構-列表/序對"))
  val IsPair: Value = Value(Symbol("列表/序對？"))
  val ConsTagged: Value = Value(Symbol("構造-具型別體"))
  val ElimTagged: Value = Value(Symbol("解構-具型別體"))
  val IsNull: Value = Value(Symbol("列表/空？"))
  val IsSymbol: Value = Value(Symbol("符號？"))
  val Rec: Value = Value(Symbol("遞歸"))
}