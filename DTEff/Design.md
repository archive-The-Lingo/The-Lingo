# Design

## size

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

B_proof: B
B_proof a = a a
// a: A -> B, a: A, so a a: B
```