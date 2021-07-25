#lang racket

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

#|
(define (_e x env undef-vars letrec-env); undef-vars: Listof Symbol ; env: Listof (Symbol * Any) ; letrec-env: => Listof (Symbol * Any)
  (cond
    ((list? x)
     (let ((f (car x)) (xs (cdr x)))
       (cond
         ((symbol? f)
          (cond
            ((memq f undef-vars) (error "eval" "the variable will be defined" f))
            ((assq f env) => (lambda (v) (apply (cdr v) (map (lambda (x) (_e x env undef-vars letrec-env)) xs))))
            ((eq? f 'quote)
             (if (= (length xs) 1)
                 (car xs)
                 (error "eval" "illegal quote" x)))
            ((eq? f 'lambda)
             (if (= (length xs) 2)
                 (let ((parsed-args (parse-args (car xs))) (body (car (cdr xs))))
                   (let ((maybe-rest (car parsed-args)) (main-pattern (cdr parsed-args)))
                     (lambda xs
                       (let ((forced-letrec-env 
|#
(define (may-force x) (if (promise? x) (force x) x))

(define (_eval x raw-env)
  (let ((env (lambda () (may-force raw-env))))
    (cond
      ((list? x)
       (let ((f (car x)) (xs (cdr x)))
         (cond
           ((eq? f 'quote)
            (if (and (pair? xs) (null? (cdr xs)))
                (car xs)
                (error "eval: illegal quote")))
           ((eq? f 'lambda)
            (if (= (length xs) 2)
                (let ((parsed-args (parse-args (car xs))) (body (car (cdr xs))))
                  (let ((maybe-rest (car parsed-args)) (main-pattern (cdr parsed-args)))
                    (lambda xs (_eval body (match-args maybe-rest main-pattern xs (env))))))
                (error "eval: illegal lambda")))
           ((eq? f 'if)
            (if (= (length xs) 3)
                (let ((b (car xs)) (x (car (cdr xs))) (y (car (cdr (cdr xs)))))
                  (if (_eval b (env)) (_eval x (env)) (_eval y (env))))
                (error "eval: illegal if")))
           ((eq? f 'letrec)
            (if (= (length xs) 2)
                (let ((parsed-lets (parse-let (car xs))) (body (car (cdr xs))))
                  (letrec ((inner-env
                            (delay
                              (append (map (lambda (element) (cons (car element) (delay (_eval (cdr element) inner-env)))) parsed-lets) (env)))))
                    (_eval body inner-env)))
                (error "eval: illegal letrec")))
           (else (apply (_eval f (env)) (map (lambda (v) (_eval v (env))) xs))))))
      ((symbol? x)
       (let ((p (assoc x (env))))
         (if (eq? p #f)
             (error "eval: no definition")
             (may-force (cdr p)))))
      ((or (integer? x) (vector? x)) x)
      (else (error "eval: illegal expression")))))

(define (parse-let xs); -> Listof (Symbol * Any)
  (if (list? xs)
      (map (lambda (element)
             (if (and (list? element) (= (length element) 2) (symbol? (car element)))
                 (cons (car element) (car (cdr element)))
                 (error "illegal let"))) xs)
      (error "illegal let")))

(define (parse-args args); -> Maybe Symbol * Listof Symbol
  (cond
    ((symbol? args) (cons args '()))
    ((null? args) (cons #f '()))
    ((pair? args)
     (if (symbol? (car args))
         (let ((r (parse-args (cdr args))))
           (cons (car r) (cons (car args) (cdr r))))
         (error "not symbol")))
    (else (error "illegal arguments pattern"))))

(define (match-args maybe-rest main-pattern xs env)
  (if (symbol? maybe-rest)
      (if (null? main-pattern)
          (cons (cons maybe-rest xs) env)
          (if (pair? xs)
              (match-args maybe-rest (cdr main-pattern) (cdr xs) (cons (cons (car main-pattern) (car xs)) env))
              (error "not enough arguments")))
      (if (null? main-pattern)
          (if (null? xs)
              env
              (error "too many arguments"))
          (if (pair? xs)
              (match-args maybe-rest (cdr main-pattern) (cdr xs) (cons (cons (car main-pattern) (car xs)) env))
              (error "not enough arguments")))))

(define (evaluate x . rest)
  (cond
    ((null? rest) (_eval x top-level))
    ((= (length rest) 1) (_eval x (car rest)))
    (else (error "evaluate: too many arguments"))))

; ------------- tests ---------

(define (test-check title tested-expression expected-result)
  (if (equal? tested-expression expected-result)
      #t
      (error (string-append "Failed:" title))))


(test-check "'(a b c)" (evaluate '(quote (a b c))) '(a b c))

(define .list '(lambda xs xs))

(test-check "(list 'a 'b 'c)" (evaluate `(,.list (quote a) (quote b) (quote c))) '(a b c))

(define .id '(lambda (x) x))

(test-check "((id (id id)) 'a)" (evaluate `((,.id (,.id ,.id)) 'a)) 'a)

(test-check "simple letrec" (evaluate `(letrec ((a b) (b 0)) a)) 0)