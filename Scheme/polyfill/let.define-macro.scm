(define-macro (let bindings . body)
  `((lambda ,(map car bindings) ,@body) ,@(map (lambda (x) (car (cdr x))) bindings)))
