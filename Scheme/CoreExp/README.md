# CoreExp

subset of r7rs

## Types

+ null
+ pair
+ procedure
+ boolean
+ symbol
+ char
+ string
+ integer
+ vector

## Syntax

+ `(f x...)`
+ `(lambda (x...) body)`
+ `(quote x)`
+ `(if b x y)`
+ `(letrec ((x y) ...) body)`

## Procedures

type error is not defined

+ `eq?`
+ `apply`

+ `car`
+ `cdr`
+ `pair?`
+ `cons`
+ `null?`

+ `boolean?`

+ `string?`
+ `string->list`
+ `list->string`

+ `char?`
+ `char->integer`
+ `integer->char`

+ `integer?`
+ `+` `-` `*` `/` (two operands)
  Division by zero is not defined
+ `=` `<` `>` `<=` `>=` (two operands)

+ `vector?`
+ `list->vector`
+ `vector->list` (one operand)
+ `vector-length`
+ `vector-ref`
