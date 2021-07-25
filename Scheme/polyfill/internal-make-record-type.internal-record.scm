; also depends on error, list, string, define, let
(define (?*make-record-type name count)
  (let ((name-str (symbol->string name)) (pred (lambda (x) (and (?*record? x) (eq? (?*record-name x) name)))))
    (list
     (lambda fields
       (if (= (length fields) count)
           (?*record name fields)
           (error name-str "arity mismatch" count fields)))
     pred
     (map (lambda (n) (lambda (x) (if (pred x) (?*record-field-ref x n) (error name-str "type mismatch" x))))
          (range count)))))
