# TheLingo

This is an experimental language trying not to assume

TheLingo don't assume:

+ Numbers are basic
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

我希望我將給該語言寫一個完整而模糊的定義

作爲一種啓示

其目的在於

將他人引導進其觀察所得的體驗中

讓他人體驗我的體驗

我知道

該處的定義十分模糊

一旦用某種人們所認爲的比較精確的語言以某種方式扭曲後寫出來

就可能出現明顯的謬誤

### 零

該語言是函數式的

函數沒有副作用

值是不可變的

### 一

我不假定 Weak Head Normal Form 是唯一的

就像某些語言

沒有定義從集合 pop 元素的順序

### 二

我允許

一個實現在計算 Weak Head Normal Form 的過程中

將確定爲永遠不能化簡爲 Weak Head Normal Form 的 Expression

解釋爲某種 Weak Head Normal Form

#### 效應

圖靈的有關停機問題證明就此失效

而該語言在直覺上仍能描述所有可計算算法

這不是說我可以證明某個實現總會結束執行

只是恰好破壞了圖靈證明時的假設：Weak Head Normal Form 是唯一的
