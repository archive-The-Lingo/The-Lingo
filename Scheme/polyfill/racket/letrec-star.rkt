(define-syntax letrec* (syntax-rules () ((_ binds body ...) (letrec binds body ...))))
