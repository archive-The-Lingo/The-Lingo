# DTEff (CoreDependentTypeEffect)

## Inspirations

+ https://github.com/koka-lang/koka
+ Coq
+ Idris / Agda

## Design

Effect: meta informations for functions

Values themself don't have effects

Examples:

+ sized
+ with assumptions
+ gradual typing
+ may not halt (div)
+ io
+ st

`n * sized n -> pure`?

`Type` is sized. How about `Effect`?
`Effect : Type 0` `div : Effect`

`Effect = Type -> Type` or a builtin feature?

### Halting

Take care of io. It is very easy to implement recursive function with io/st.

io/st implies div?

letrec -- data (maybe including closures) / function only

funtion only letrec can easily use `sized`. What about data? be lazy and mark them as `div`?

Is there any other ways to ensure halting? force Weak head normal form when doing recursive.

`size` is just like how `halt` is defined - `halt` in finite steps - which is `size`

[Agda sized](https://agda.readthedocs.io/en/v2.5.2/language/sized-types.html)

Does size make sense with some effect?

What does size mean in the effect world? - `return` has the size 0 and `x >>= f ` has a size bigger than x and f - and allow any recursion with effect?

#### Examples

assume
```
x -- 100
a -- 9
b -- 10
```

```
return "a" -- 0
print "a" >>= \_ => return "a" -- 1
if x then a else b -- 110 ; could be 100 if the purpose of size is only to ensure it will be a weak head normal form?
```

#### Coinduction

[Coinduction - Agda](https://web.archive.org/web/20201210063822/https://agda.readthedocs.io/en/latest/language/coinduction.html)

Too hard to implement this feature?

### Core language

Should the core language include the complete Effect system? or just a few special effects as `Type -> Type` (`sized` for instance)?

complete Effect?

Effect:

+ builtin flags
+ custom flags
+ traditional Effect

building block:

effect operator - 
>>=
...

How to model and describe effect operators? - Is it still possible to create an expression-only language?
operator name + type
example:
print + `String -> ()`

## Examples

Explicit version
```
id: {e: Effect, n: Nat, t: Type n}, t -> e t
id {e, n, t} x = x

id_pure: {n: Nat, t: Type n}, t -> pure t
id_pure {n, t} x = x 

id_e: {e: Effect, n: Nat, t: Type n}, (() -> e t) -> e t
id_e {e, n, t} f = f ()


map: {e: Effect, n0: Nat, t0: Type n0, n1: Nat, t1: Type n1}, Listof t -> (t -> e t1) -> e (Listof t1)
...
```

Simple version
```
id: t -> t
id x = x

map: Listof t -> (t -> t1) -> Listof t1
...
```

with size

TODO

procedural language like

TODO