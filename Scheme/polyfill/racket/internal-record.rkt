(struct ?*record (name fields) #:transparent #:guard (lambda (name fields _) (if (and (symbol? name) (list? fields)) (values name fields) (error "illegal ?*record"))))
(define (?*record-field-ref r i) (list-ref (?*record-fields r) i))
