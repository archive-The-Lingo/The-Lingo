## Design 0

### size

Mathematical induction - The base case can not be missing

So do sets, functions and types

Constructivism - One can not assume that there is `Type` and then construct `Type` so that `Type: Type`

consider 
```
Type = (n: Nat * Type n)
```
```
Type 0: Type 1: Type 2 ...
```

but `Type: Type` does not hold

Related: [Why must inductive types be strictly positive?](http://web.archive.org/web/20210620141510/https://vilhelms.github.io/posts/why-must-inductive-types-be-strictly-positive/)
```
// Curry's paradox
A: Type
A = A -> B
// or `data A = A (A -> B)`

B_proof: B
B_proof a = a a
// a: A -> B, a: A, so `a a: B`
```

### Examples

```

```

## 1

又是无用的列表

+ Koka algebraic effects ...
+ ΠΣ Structural Type ...
  ```
  Nat: Type = (l: {zero succ}) * case l of { zero -> {unit} | suc -> [Nat]};
  ```

...

References

[A Framework for Dependent Types and Effects](https://arxiv.org/abs/1512.08009) 不过这篇论文有关吗？

> On the other hand, we hope it can contribute a small-step towards the ultimate goal of an elegant fully fledged language for certified effectful programming. 


## 2

There are two types of type markers.

+ function marker
  + io
+ type marker
  + erased at runtime / linear
  + not fully type checked


+ Effect
  + io
+ Function Attribute
  + div?
  + sized?
+ Value Attribute
  + erased at runtime / linear
  + not fully type checked
  + sized?
