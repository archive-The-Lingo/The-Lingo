# Design

bidirectional type checking.

## ==

`type: Value * (Environment * val1: Expression) * (Environment * val2: Expression)` ?

## Any n

`t: U n * v: t`

## Write Type Checker in FullRel or FullFP on the top of FullRel

Every expression has multiple possible types in this design, especially symbols, which makes it difficult to implement the type synthesis.
