# TheLingo

This is an experimental language

TheLingo don't assume:

+ Docs and codes are mainly written in English
+ Time can be represented in unsigned 64-bit integer
+ Users are all on one planet, and the network delay won't exceed 24h
+ Internet exist
+ Users only use Windows, UNIX-like OS and/or realtime OS based on C
+ Users only have electronic computers and quantum computers, no other types of computers
+ More

TheLingo assume for now:

+ Lisp's symbols are natural to identify things

## Implementation

1. Implement the core language in a language for electronic computers
1. Implement some experimental languages in the core language

## Status

I rewrote the core language hundreds of times and I am still rewriting it.

## 注

我希望我將給該語言寫一個完整而模糊的敘述 作爲一種啓示

其目的在於

將他人引導進其觀察所得的體驗中 讓他人體驗我的體驗

該目的若能達到

那麼此處的敘述也是無足輕重的

因此該處的敘述不應該作爲戒律

----------

我知道 該處的敘述十分模糊

一旦用某種人們所認爲的比較精確的語言以某種方式扭曲後寫出來 就可能出現明顯的謬誤

### 零

該語言是函數式的

函數只依賴輸入的值沒有副作用 值是不可變的

計算/`reduce` 爲 有方向的變化 （`directed change`）

表達式不一定有`Canonical Form` 比如 從集合中`pop`元素

`Weak Head Normal Form`不是唯一的

故該語言不具有`Referential transparency`

#### 零 一

該描述是爲使用者而寫

例：

具體實現之時 值可以是可變的 只要使用者觀測不到

### 一

一個表達式描述它所有可能的`Weak Head Normal Form`

### 二

我允許

一個實現在計算`Weak Head Normal Form`的過程中

將確定爲永遠不能化簡爲`Weak Head Normal Form`的表達式

解釋爲某種表示`Exception`的`Weak Head Normal Form`

#### 二 一

解釋時儘量把最小的子表達式爲`Exception`

否則其`Exception`處理系統將失效 整個程式被解釋爲一個簡單的`Exception`

----------
效應：

圖靈的有關停機問題證明就此失效

而該語言在直覺上仍能描述所有可計算算法

這不是說我可以證明某個實現總會結束執行

只是恰好破壞了圖靈證明時的假設：`Weak Head Normal Form`是唯一的

### 四

`library`應有確定的`介面` -- 既不更改 也盡量不增加

當不得已要更改`介面`時 應視爲新的`library`

### 五

一個完整的`library`或程式應包含它需要的所有代碼 不依賴可以在任何時刻消失的`URL`

因此 `包管理器`作爲更新依賴的工具 不再是下載依賴的工具

