;#lang racket
; GNU Guile / Chez Scheme / BiwaScheme / Racket
; BiwaScheme does not support `(error)`
; Racket does not support letrec*
;(define (error . xs) (raise xs)); BiwaScheme
;(define-syntax letrec* (syntax-rules () ((_ binds body) (letrec binds body)))); Racket

(define (_eq? x y) (eq? x y))
(define (_apply f xs) (apply f xs))
(define (_car x) (car x))
(define (_cdr x) (cdr x))
(define (_pair? x) (pair? x))
(define (_cons x y) (cons x y))
(define (_null? x) (null? x))
(define (_boolean? x) (boolean? x))
(define (_string? x) (string? x))
(define (_string->list x) (string->list x))
(define (_list->string x) (list->string x))
(define (_char? x) (char? x))
(define (_char->integer x) (char->integer x))
(define (_integer->char x) (integer->char x))
(define (_integer? x) (integer? x))
(define (_+ x y) (+ x y))
(define (_- x y) (- x y))
(define (_* x y) (* x y))
(define (_/ x y) (/ x y))
(define (_= x y) (= x y))
(define (_< x y) (< x y))
(define (_> x y) (> x y))
(define (_<= x y) (<= x y))
(define (_>= x y) (>= x y))
(define (_vector? x) (vector? x))
(define (_list->vector x) (list->vector x))
(define (_vector->list x) (vector->list x))
(define (_vector-length x) (vector-length x))
(define (_vector-ref v x) (vector-ref v x))
#|
(define (?*make-record-type name count)
  (let-values (((struct: make ? ref set!) (make-struct-type name #f count 0)))
    (list make ? (map (lambda (n) (lambda (x) (ref x n))) (range count)))))
|#

(define top-level
  (list
   (cons 'eq? _eq?)
   (cons 'apply _apply)
   (cons 'car _car)
   (cons 'cdr _cdr)
   (cons 'pair? _pair?)
   (cons 'cons _cons)
   (cons 'null? _null?)
   (cons 'boolean? _boolean?)
   (cons 'string? _string?)
   (cons 'string->list _string->list)
   (cons 'list->string _list->string)
   (cons 'char? _char?)
   (cons 'char->integer _char->integer)
   (cons 'integer->char _integer->char)
   (cons 'integer? _integer?)
   (cons '+ _+)
   (cons '- _-)
   (cons '* _*)
   (cons '/ _/)
   (cons '= _=)
   (cons '< _<)
   (cons '> _>)
   (cons '<= _<=)
   (cons '>= _>=)
   (cons 'vector? _vector?)
   (cons 'list->vector _list->vector)
   (cons 'vector->list _vector->list)
   (cons 'vector-length _vector-length)
   (cons 'vector-ref _vector-ref)))

(define empty-mapping '())
(define (mapping-assoc k m) (assq k m))
(define (mapping-map f m) (map f m))
(define (mapping-merge m1 m2) (append m1 m2))
(define (mapping-updated k v m) (cons (cons k v) m))
(define (mapping-remove k m)
  (cond
    ((null? m) '())
    ((pair? m)
     (let ((a (car m)) (d (cdr m)))
       (if (eq? k (car a))
           (mapping-remove k d)
           (cons a (mapping-remove k d)))))
    (else (error "mapping-remove" "illegal mapping" m))))

(define (no-duplicate0 x history)
  (cond
    ((symbol? x) (and (not (memv x history)) history))
    ((pair? x) (let ((a (no-duplicate0 (car x) history)))
                 (and a (no-duplicate0 (cdr x) a))))
    (else history)))
(define (no-duplicate x) (no-duplicate0 x '()))

(define (check-args args)
  (cond
    ((symbol? args) #t)
    ((null? args) #t)
    ((pair? args) (and (symbol? (car args)) (check-args (cdr args))))
    (else #f)))
(define (apply-args args xs env)
  (cond
    ((pair? args)
     (if (pair? xs)
         (apply-args (cdr args) (cdr xs) (mapping-updated (car args) (car xs) env))
         (error "apply" "not enough arguments")))
    ((null? args)
     (if (null? xs)
         env
         (error "apply" "too many arguments")))
    ((symbol? args) (mapping-updated args xs env))
    (else (error "apply" "illegal arguments pattern"))))

(define (letrec-env->env letrec-env) (mapping-map (lambda (e) (cons (car e) ((cdr e)))) letrec-env))

(define (_e x letrec-env env low-letrec-env); env: Listof (Symbol * Any) ; letrec-env, low-letrec-env: Listof (Symbol * => Any)
  ; todo: check duplicates between letrec-env and low-letrec-env
  (cond
    ((null? x) (error "eval" "null"))
    ((symbol? x)
     (cond
       ((mapping-assoc x letrec-env) (error "eval" "the variable will be defined" x))
       ((mapping-assoc x env) => cdr)
       ((mapping-assoc x low-letrec-env) (error "eval" "the variable will be defined" x))
       (else (error "eval" "the variable is not defined" x))))
    ((or (vector? x) (integer? x) (char? x) (string? x)) x)
    ((list? x)
     (let ((f (car x)) (xs (cdr x)))
       (cond
         ((symbol? f)
          (cond
            ((mapping-assoc f letrec-env) (error "eval" "the variable will be defined" f))
            ((mapping-assoc f env) => (lambda (v) (apply (cdr v) (map (lambda (x) (_e x letrec-env env low-letrec-env)) xs))))
            ((mapping-assoc f low-letrec-env) (error "eval" "the variable will be defined" f))
            ((eq? f 'quote)
             (if (= (length xs) 1)
                 (car xs)
                 (error "eval" "illegal quote" x)))
            ((eq? f 'lambda)
             (if (= (length xs) 2)
                 (let ((args (car xs)) (body (car (cdr xs))))
                   (if (and (check-args args) (no-duplicate args))
                       (lambda xs (_e body empty-mapping (apply-args args xs (mapping-merge (letrec-env->env letrec-env) (mapping-merge (letrec-env->env low-letrec-env) env))) empty-mapping))
                       (error "eval" "illegal arguments pattern")))
                 (error "eval" "illegal lambda")))
            ((eq? f 'letrec)
             (if (= (length xs) 2)
                 (let ((parsed-lets (parse-let (car xs))) (body (car (cdr xs))))
                   (if (no-duplicate (map car parsed-lets))
                       (letrec* ((inner-letrec-env (mapping-merge (map (lambda (x) (let ((name (car x))) (cons name (lambda () (cdr (mapping-assoc name inner-env)))))) parsed-lets) letrec-env))
                                (inner-env (map (lambda (x) (cons (car x) (_e (cdr x) inner-letrec-env env low-letrec-env))) parsed-lets)))
                         (_e body letrec-env (mapping-merge inner-env env) low-letrec-env))
                       (error "eval" "illegal letrec pattern" x)))
                 (error "eval" "illegal letrec" x)))
            ((eq? f 'letrec*)
             (if (= (length xs) 2)
                 (let ((parsed-lets (parse-let (car xs))) (body (car (cdr xs))))
                   (if (no-duplicate (map car parsed-lets))
                       (letrec* ((inner-letrec-env (mapping-merge (map (lambda (x) (let ((name (car x))) (cons name (lambda () (cdr (mapping-assoc name inner-env)))))) parsed-lets) letrec-env))
                                (make-inner-env (lambda (parsed-lets previous-letrec-env previous-result)
                                                  (if (null? parsed-lets)
                                                      previous-result
                                                      (let* ((this-let (car parsed-lets))
                                                             (this-name (car this-let))
                                                             (this-exp (cdr this-let))
                                                             (more-parsed-lets (cdr parsed-lets))
                                                             (this-env (mapping-merge previous-result env)))
                                                        (make-inner-env more-parsed-lets (mapping-remove this-name previous-letrec-env) (mapping-updated this-name (_e this-exp previous-letrec-env this-env low-letrec-env) previous-result))))))
                                (inner-env (make-inner-env parsed-lets inner-letrec-env empty-mapping)))
                         (_e body letrec-env (mapping-merge inner-env env) low-letrec-env))
                       (error "eval" "illegal letrec* pattern" x)))
                 (error "eval" "illegal letrec*" x)))
            ((eq? f '?*letrec-define*)
             (if (= (length xs) 2)
                 (let ((parsed-lets (parse-let (car xs))) (body (car (cdr xs))))
                   (if (no-duplicate (map car parsed-lets))
                       (letrec* ((inner-low-letrec-env (mapping-merge (map (lambda (x) (let ((name (car x))) (cons name (lambda () (cdr (mapping-assoc name inner-env)))))) parsed-lets) low-letrec-env))
                                (make-inner-env (lambda (parsed-lets previous-low-letrec-env previous-result)
                                                  (if (null? parsed-lets)
                                                      previous-result
                                                      (let* ((this-let (car parsed-lets))
                                                             (this-name (car this-let))
                                                             (this-exp (cdr this-let))
                                                             (more-parsed-lets (cdr parsed-lets))
                                                             (this-env (mapping-merge previous-result env)))
                                                        (make-inner-env more-parsed-lets (mapping-remove this-name previous-low-letrec-env) (mapping-updated this-name (_e this-exp letrec-env this-env previous-low-letrec-env) previous-result))))))
                                (inner-env (make-inner-env parsed-lets inner-low-letrec-env empty-mapping)))
                         (_e body letrec-env (mapping-merge inner-env env) low-letrec-env))
                       (error "eval" "illegal ?*letrec-define* pattern" x)))
                 (error "eval" "illegal ?*letrec-define*" x)))
            ((eq? f 'if)
             (if (= (length xs) 3)
                 (let ((b (car xs)) (x (car (cdr xs))) (y (car (cdr (cdr xs)))))
                   (if (_e b letrec-env env low-letrec-env) (_e x letrec-env env low-letrec-env) (_e y letrec-env env low-letrec-env)))
                 (error "eval" "illegal if")))))
         (else (apply (_e f letrec-env env low-letrec-env) (map (lambda (x) (_e x empty-mapping env low-letrec-env)) xs))))))
    (else (error "eval" "illegal expression" x))))

(define (parse-let xs); -> Listof (Symbol * Any)
  (if (list? xs)
      (map (lambda (element)
             (if (and (list? element) (= (length element) 2) (symbol? (car element)))
                 (cons (car element) (car (cdr element)))
                 (error "eval" "illegal let"))) xs)
      (error "eval" "illegal let")))

(define (evaluate x . rest)
  (cond
    ((null? rest) (_e x empty-mapping top-level empty-mapping))
    ((= (length rest) 1) (_e x empty-mapping (car rest) empty-mapping))
    (else (error "evaluate" "too many arguments" (cons x rest)))))

; ------------- tests ---------

(define (test-check title tested-expression expected-result)
  (if (equal? tested-expression expected-result)
      #t
      (error "Failed:" title tested-expression expected-result)))

(test-check "'(a b c)" (evaluate '(quote (a b c))) '(a b c))

(define list- '(lambda xs xs))

(test-check "(list 'a 'b 'c)" (evaluate `(,list- (quote a) (quote b) (quote c))) '(a b c))

(define id- '(lambda (x) x))

(test-check "((id (id id)) 'a)" (evaluate `((,id- (,id- ,id-)) 'a)) 'a)

(test-check
 "simple letrec"
 (evaluate
  '(letrec ((foo (lambda () bar))
            (bar 7))
     (cons (foo) bar)))
 '(7 . 7))

(test-check
 "simple letrec*"
 (evaluate
  '(letrec* ((foo (lambda () bar))
            (bar 7))
     (cons (foo) bar)))
 '(7 . 7))

(test-check "basic ?*letrec-define*" (evaluate '(?*letrec-define* ((origin+ +) (+ (lambda (x y) (origin+ x y)))) (+ 1 2))) 3)