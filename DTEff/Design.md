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