# internal-letrec-define-star

## Syntax

+ `?*letrec-define*`
  A `letrec*` that behaves like `define`s
  Example:
  `(?*letrec-define* ((origin+ +) (+ (lambda (x y) (origin+ x y)))) (+ 1 2))`
  `(define origin+ +) (define (+ x y) (origin+ x y)) (+ 1 2)`
  A partial implementation is `(let* (...) (letrec* (...) ...))`. This implementation works most of time and will report error when it can not handle the situation
