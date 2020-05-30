/*
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package the.lingo

final object Symbols {
  val Core:Sym = '核
  val Id: Sym = '識別子
  val Exp: Sym = '式
  val ApplyFunc: Sym = Sym("用-函式")
  val Macro: Sym = '構式子
  val Quote: Sym = '引
  val Func: Sym = '函式
  val ApplyMacro: Sym = Sym("用-構式子")
  val Comment: Sym = '注釋
  val Exception: Sym = '異常
  val Eval: Sym = '解算
  val Mapping: Sym = '映射
  val Builtin: Sym = '內建
  val False: Sym = '陰
  val True: Sym = '陽
  val Char: Sym = '字符
  val String: Sym = '字串
  val Nat: Sym = '自然數
  val NatZero: Sym = Sym("自然數/零")
  val NatSucc: Sym = Sym("自然數/加一")
  val IsPair: Sym = Sym("序列/序對？")
  val ConsPair: Sym = Sym("構造-序列/序對")
  val ElimPair: Sym = Sym("解構-序列/序對")
  val IsTagged: Sym = Sym("具型別？")
  val ConsTagged: Sym = Sym("構造-具型別")
  val ElimTagged: Sym = Sym("解構-具型別")
  val IsBottom: Sym = Sym("異常？")
  val ConsBottom: Sym = Sym("構造-異常")
  val ElimBottom: Sym = Sym("解構-異常")
  val IsNull: Sym = Sym("序列/空？")
  val IsSymbol: Sym = Sym("符號？")
  val SymbolToString: Sym = Sym("符號→字串")
  val StringToSymbol: Sym = Sym("字串→符號")
  val Rec: Sym = '遞歸
  val Positioned: Sym = '具位置
}