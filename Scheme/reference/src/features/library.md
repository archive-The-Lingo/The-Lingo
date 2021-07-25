# library

depends on [define](define)

## Syntax

+ `(define-library <name> <declaration> ...)`
  based on r7rs and r6rs
  declaration:
  + `(export <export spec> ...)`
  + `(import <import set> ...)`
  + command or definition
  + `(include <filename> ...)`
