(define (?*make-record-type name count)
  (let-values (((struct: make ? ref set!) (make-struct-type name #f count 0)))
    (list make ? (map (lambda (n) (lambda (x) (ref x n))) (range count)))))
