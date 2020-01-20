#| Copyright (C) 2017-2020  ㄗㄠˋ ㄑㄧˊ <tsao-chi@the-lingo.org> |#
#lang racket

;; This file is written in a Racket dialect defined and described below
{define-syntax-rule {if-typecheck-on t f} t}
#|
  cons-    constructor
  elim-    eliminator
  eq    equal
  nat    natural numbers
  exp    expression
  val    value
  pred    predicate
  op    optimized
  { .. }    macro/syntax
  /t -t t    type
  r -r    result/return
  -tt    a function returning a type
  -*    all
  !    read/write mutable value(s)
  :    type annotation
  -m    monad
  -aux    auxiliary
  -s    symbol
  -f    function
  -v    value
  x y v    A variable (named like math)
  s  (plural)  list
|#
{require racket/contract}
{require (rename-in racket [cond rkt#%cond] [define rkt#%define] [define/contract rkt#%define/contract])}
{define-syntax lambda
  {syntax-rules ()
    [(_ (arg ...) . body) {match-lambda* [(list arg ...) . body]}]
    [(_ (arg ... . rest-id) . body) {match-lambda* [(list-rest arg ... rest-id) . body]}]}}
{define-syntax-rule {λ . r} {lambda . r}}
{define-syntax-rule {let . r} {match-let . r}}
{define-syntax define
  {syntax-rules ()
    [(_ (head . args) . body) {define head {λ args . body}}]
    [(_ id expr) {rkt#%define id expr}]}}
{define-syntax define/contract
  {syntax-rules ()
    [(_ (head . args) contract-expr . body) {define/contract head contract-expr {λ args . body}}]
    [(_ id contract-expr expr) {rkt#%define/contract id contract-expr expr}]}}
{define-syntax cond
  {syntax-rules (else =>)
    [(_ head ... [else . then-body]) {rkt#%cond head ... [else . then-body]}]
    [(_ head ...) {rkt#%cond head ... [else (assert-unreachable)]}]}}
{require (only-in typed/racket assert)}
{define-syntax-rule {define:type . xs} {define . xs}}
{define-syntax-rule {define-syntax-rule:type . xs} {define-syntax-rule . xs}}
{if-typecheck-on
 {define-syntax-rule {define/t . xs} {define/contract . xs}}
 {define-syntax-rule {define/t n t . xs} {define n . xs}}}
{if-typecheck-on
 {define (: val typ) {define/t tmp typ val} tmp}
 {define (: val typ) val}}
{if-typecheck-on
 {define-syntax-rule {let/t ([id typ val] ...) . r}
   {match-let ([id {: val typ}] ...) . r}}
 {define-syntax-rule {let/t ([id typ val] ...) . r}
   {match-let ([id val] ...) . r}}}
{define-syntax-rule (t->? t) ;; type -> predicate
  {let ([t-lazy (delay t)])
    {λ (x)
      {with-handlers ([exn:fail:contract? {λ (e) #f}])
        {define/contract _ (force t-lazy) x}
        #t}}}}
{define:type and-tt and/c}
{define:type or-tt or/c}
{define:type not-tt not/c}
{define:type any-t any/c}
{define:type string-t string?}
{define:type vector-tt vector/c}
{define:type void-t void?}
{define:type boolean-t boolean?}
{define:type box-t box/c}
{define:type list-of-tt listof}
{define:type set-of-tt set/c}
{define:type null-t null?}
{define:type symbol-t symbol?}
{define:type char-t char?}
{define:type nat-t exact-nonnegative-integer?}
{define:type nothing-t void-t}
{define/t nothing nothing-t (void)}
{define/t (nothing? x)
  (-> any-t boolean-t)
  (equal? x nothing)}
{define (assert-unreachable) (error 'assert-unreachable)}
{define-syntax-rule (TODO . _) (error 'WIP)}
{define (id x) x}
{define point-eq? eq?}
{define/t (nat-eq? x y)
  (-> nat-t nat-t boolean-t)
  (= x y)}
{define string-eq? string=?}
{define/t (symbol-eq? x y)
  (-> symbol-t symbol-t boolean-t)
  (eq? x y)}
{define (memorize1 f)
  {let ([cache (make-weak-hasheq)])
    {λ (x)
      (force (hash-ref! cache x {λ () (delay (f x))}))}}}
{define-syntax do
  {syntax-rules (<- :=)
    [(_ >>= x) x]
    [(_ >>= #{x := v} . r) {let ([x v]) {do >>= . r}}]
    [(_ >>= #{x <- v} . r) (>>= v {λ (x) {do >>= . r}})]
    [(_ >>= v . r) {do >>= #{x <- v} . r}]}}

{define:type (cont-tt a r) (-> (-> a r) r)}
{define/t (cont-return x)
  (-> any-t (cont-tt any-t any-t))
  {λ (c) (c x)}}
{define/t (cont->>= x f)
  (-> (cont-tt any-t any-t) (-> any-t (cont-tt any-t any-t)) (cont-tt any-t any-t))
  {λ (c) (x {λ (v) ((f v) c)})}}
{define-syntax-rule {cont-if-return-m b v}
  {λ (c) (if b v (c nothing))}}
{define (map-m f xs)
  (if (null? xs)
      (cont-return '())
      {do cont->>=
        #{head <- (f (car xs))}
        #{tail <- (map-m f (cdr xs))}
        (cont-return (cons head tail))})}

{define:type t-id-t symbol-t}
{define t-id-eq? eq?}
{define:type value-bone-t (vector-tt t-id-t any-t any-t any-t)}

{define:type value-symbol-t-id-t (and-tt t-id-t 'symbol)}
{define/t value-symbol-t-id value-symbol-t-id-t 'symbol}
{define:type value-pair-t-id-t (and-tt t-id-t 'pair)}
{define/t value-pair-t-id value-pair-t-id-t 'pair}
{define:type value-null-t-id-t (and-tt t-id-t 'null)}
{define/t value-null-t-id value-null-t-id-t 'null}
{define:type value-struct-t-id-t (and-tt t-id-t 'struct)}
{define/t value-struct-t-id value-struct-t-id-t 'struct}
{define:type value-just-t-id-t (and-tt t-id-t 'just)}
{define/t value-just-t-id value-just-t-id-t 'just}
{define:type value-delay-t-id-t (and-tt t-id-t 'delay)}
{define/t value-delay-t-id value-delay-t-id-t 'delay}
{define:type value-optimized-t-id-t (and-tt t-id-t 'optimized)}
{define/t value-optimized-t-id value-optimized-t-id-t 'optimized}

;; without t->?, it will make the same value different and disallow changing the type of value
{define-syntax-rule:type (value-tt x) (t->? (and-tt value-bone-t x))}
{define:type value-symbol-t (value-tt (vector-tt value-symbol-t-id-t string-t nothing-t nothing-t))}
{define:type value-pair-t (value-tt (vector-tt value-pair-t-id-t value-t value-t nothing-t))}
{define:type value-null-t (value-tt (vector-tt value-null-t-id-t nothing-t nothing-t nothing-t))}
{define:type value-struct-t (value-tt (vector-tt value-struct-t-id-t value-t value-t nothing-t))}
{define:type value-just-t (value-tt (vector-tt value-just-t-id-t value-t nothing-t nothing-t))}
{define:type value-delay-t
  (value-tt
   (vector-tt
    value-delay-t-id-t
    (-> value-t) ;; exec-f
    (-> (vector-tt identifierspace-t value-t (list-of-tt value-t))) ;; display-f
    nothing-t))}
{define:type value-optimized-t
  (value-tt
   (vector-tt
    value-optimized-t-id-t
    (-> any-t value-t) ;; Returns the equivalent value without this layer of optimization (optimizations can have many layers)
    symbol-t ;; tag
    any-t ;; value
    ))}
;; value-t type values may change, but often it is always a value-t type value, so it is memorized
{define:type value-t
  (memorize1
   (t->?
    (or-tt
     value-symbol-t
     value-pair-t
     value-null-t
     value-struct-t
     value-just-t
     value-delay-t
     value-optimized-t
     )))}

{define-values (identifierspace-t identifierspace? identifierspace-null identifierspace-ref identifierspace-set identifierspace->list)
  ({λ ()
     {define (val-eq? x y) (value-force+equal? x y)} ;; because "cannot reference an identifier before its definition"
     {define-custom-hash-types identifierspace
       #:key? (t->? value-t)
       val-eq?}
     {define is? ({λ () {define (identifierspace? x) (immutable-identifierspace? x)} identifierspace?})}
     {define:type identifierspace-t identifierspace?}
     {define identifierspace-null (make-immutable-identifierspace '())}
     {define/t (identifierspace-ref dict key failure-result)
       (-> identifierspace-t value-t (-> any-t) any-t)
       (dict-ref dict key failure-result)}
     {define/t (identifierspace-set dict key v)
       (-> identifierspace-t value-t any-t identifierspace-t)
       (dict-set dict key v)}
     {define/t (identifierspace->list x)
       (-> identifierspace-t (list-of-tt (vector-tt value-t any-t)))
       (map {λ (v) (vector (car v) (cdr v))} (dict->list x))}
     (values identifierspace-t is? identifierspace-null identifierspace-ref identifierspace-set identifierspace->list)})}

{define/t (op? (vector t _ ...))
  (-> value-t boolean-t)
  (t-id-eq? t value-optimized-t-id)}
{define/t (cons-op un-op-f tag value)
  (-> (-> any-t value-t) symbol-t any-t value-optimized-t)
  (vector value-optimized-t-id un-op-f tag value)}
{define/t (un-op (vector _  un-op tag value _ ...))
  (-> value-optimized-t value-t)
  (un-op value)}
{define/t (un-op* x)
  (-> value-t value-t)
  (if (op? x)
      (un-op* (un-op x))
      x)}
{define/t (op-tag (vector _ un-op-f tag value _ ...))
  (-> value-optimized-t symbol-t)
  tag}
{define/t (op-value (vector _ un-op-f tag value _ ...))
  (-> value-optimized-t any-t)
  value}
{define/t (->op/_? s x)
  (-> symbol-t value-t boolean-t)
  {and (op? x)
       (symbol-eq? s (op-tag x))}}

{define (value-_? s v)
  (-> t-id-t value-t boolean-t)
  (if (op? v)
      (value-_? s (un-op v))
      {let ([(vector t _ ...) v])
        (t-id-eq? t s)})}

{define/t (value-symbol? x)
  (-> value-t boolean-t)
  (value-_? value-symbol-t-id x)}
{define/t (value-pair? x)
  (-> value-t boolean-t)
  (value-_? value-pair-t-id x)}
{define/t (value-null? x)
  (-> value-t boolean-t)
  (value-_? value-null-t-id x)}
{define/t (value-struct? x)
  (-> value-t boolean-t)
  (value-_? value-struct-t-id x)}
{define/t (value-just? x)
  (-> value-t boolean-t)
  (value-_? value-just-t-id x)}
{define/t (value-delay? x)
  (-> value-t boolean-t)
  (value-_? value-delay-t-id x)}

{define (DEBUG m ... v) (displayln m) v} ;; TODO: Implement a complete error handling system

{define/t (cons-value-symbol x)
  (-> string-t value-symbol-t)
  (vector value-symbol-t-id x nothing nothing)}
{define/t (aux-elim-value-symbol (vector _ v _ ...))
  (-> value-symbol-t string-t)
  v}
{define (elim-value-symbol x) (aux-elim-value-symbol (un-op* x))}
{define/t (value-symbol-equal? x y)
  (-> value-symbol-t value-symbol-t boolean-t)
  (string-eq? (elim-value-symbol x) (elim-value-symbol y))}
{define/t (cons-value-pair x y)
  (-> value-t value-t value-pair-t)
  (vector value-pair-t-id x y nothing)}
{define/t (aux-elim-value-pair (vector _ x y _ ...))
  (-> value-pair-t (vector-tt value-t value-t))
  (vector x y)}
{define (elim-value-pair x) (aux-elim-value-pair (un-op* x))}
{define/t (cons-value-struct x y)
  (-> value-t value-t value-struct-t)
  (vector value-struct-t-id x y nothing)}
{define/t (aux-elim-value-struct (vector _ x y _ ...))
  (-> value-struct-t (vector-tt value-t value-t))
  (vector x y)}
{define (elim-value-struct x) (aux-elim-value-struct (un-op* x))}
{define/t value-null value-null-t (vector value-null-t-id nothing nothing nothing)}
{define/t (cons-value-delay exec-f display-f)
  (-> (-> value-t) (-> (vector-tt identifierspace-t value-t (list-of-tt value-t))) value-delay-t)
  (vector value-delay-t-id exec-f display-f nothing)}
{define/t (aux-elim-value-delay (vector _ exec-f display-f _ ...))
  (-> value-delay-t (vector-tt (-> value-t) (-> (vector-tt identifierspace-t value-t (list-of-tt value-t)))))
  (vector exec-f display-f)}
{define (elim-value-delay x) (aux-elim-value-delay (un-op* x))}
{define/t (aux-must-value-unjust-1 (vector _ v _ ...))
  (-> value-just-t value-t)
  v}
{define (must-value-unjust-1 x) (aux-must-value-unjust-1 (un-op* x))}

{define:type op/nat-data-t nat-t}
{define op/nat-type-s 'nat}
{define/t (op/nat-data->value n)
  (-> nat-t value-t)
  (if (nat-eq? n 0)
      (cons-value-struct nat-zero-s value-null)
      (cons-value-struct nat-succ-s (cons-op/nat (- n 1))))}
{define/t (cons-op/nat n)
  (-> nat-t value-t)
  (cons-op op/nat-data->value op/nat-type-s n)}
{define/t (op/nat? x)
  (-> value-t boolean-t)
  (->op/_? op/nat-type-s x)}

{define/t (elim-value-struct-fixed-type-and-length-or-return-m type len x ->error-v display-f)
  (->
   value-symbol-t
   nat-t
   value-t
   (-> value-t)
   (-> (vector-tt identifierspace-t value-t (list-of-tt value-t)))
   (cont-tt (list-of-tt value-t) value-t))
  {do cont->>=
    #{x <- (value-undelay-m x display-f)}
    (cont-if-return-m (not (value-struct? x)) (DEBUG "not struct, require"type", get"x (->error-v)))
    #{(vector x-type x-list) := (elim-value-struct x)}
    (cont-if-return-m (not (value-equal? x-type type)) (DEBUG "struct type error" (->error-v)))
    #{x-list <- (value-undelay-list-or-return-m x-list ->error-v display-f)}
    (cont-if-return-m (not (nat-eq? (length x-list) len)) (DEBUG type": struct length error, require"len", get"(length x-list)". x="x (->error-v)))
    (cont-return x-list)}}

{define/t (value->nat-or-return-m x ->error-v display-f)
  (->
   value-t
   (-> value-t)
   (-> (vector-tt identifierspace-t value-t (list-of-tt value-t)))
   (cont-tt nat-t value-t))
  {do cont->>=
    #{x <- (value-undelay-m x display-f)}
    {cond
      [(op/nat? x) (cont-return (op-value x))]
      [(not (value-struct? x)) (cont-return (DEBUG "not struct" (->error-v)))]
      [else
       {do cont->>=
         #{(vector x-type x-list) := (elim-value-struct x)}
         #{x-type <- (value-undelay-m x-type display-f)}
         {cond
           [(value-equal? x-type nat-zero-s)
            {do cont->>=
              #{x-list <- (value-undelay-list-or-return-m x-list ->error-v display-f)}
              (cont-if-return-m (not (null? x-list)) (DEBUG "struct length error" (->error-v)))
              (cont-return 0)}]
           [(value-equal? x-type nat-succ-s)
            {do cont->>=
              #{x-list <- (value-undelay-list-or-return-m x-list ->error-v display-f)}
              (cont-if-return-m (not (nat-eq? (length x-list) 1)) (->error-v))
              #{(list x-v) := x-list}
              #{x-v-r <- (value->nat-or-return-m x-v ->error-v display-f)}
              (cont-return (+ x-v-r 1))}]
           [else (cont-return (DEBUG "struct type error" (->error-v)))]}}]}}}

{define/t (char->value c)
  (-> char-t value-t)
  (cons-value-struct
   char-s
   (cons-value-list
    (cons-op/nat (char->integer c))))}
{define/t (string->value s)
  (-> string-t value-t)
  (cons-value-struct
   string-s
   (cons-value-list
    (list->value (map char->value (string->list s)))))}

{define/t (value->char-or-return-m x ->error-v display-f)
  (->
   value-t
   (-> value-t)
   (-> (vector-tt identifierspace-t value-t (list-of-tt value-t)))
   (cont-tt char-t value-t))
  {do cont->>=
    #{(list x-v) <- (elim-value-struct-fixed-type-and-length-or-return-m char-s 1 x ->error-v display-f)}
    #{x-v-nat <- (value->nat-or-return-m x-v ->error-v display-f)}
    (cont-return (integer->char x-v-nat))}} ;; TODO: handle chars not in the range (or/c (integer-in 0 55295) (integer-in 57344 1114111))
{define/t (value->string-or-return-m x ->error-v display-f)
  (->
   value-t
   (-> value-t)
   (-> (vector-tt identifierspace-t value-t (list-of-tt value-t)))
   (cont-tt string-t value-t))
  {do cont->>=
    #{(list x-v) <- (elim-value-struct-fixed-type-and-length-or-return-m string-s 1 x ->error-v display-f)}
    #{x-v <- (value-undelay-list-or-return-m x-v ->error-v display-f)}
    #{x-v-char <- (map-m {λ (x) (value->char-or-return-m x ->error-v display-f)} x-v)}
    (cont-return (list->string x-v-char))}}

{define/t list->value
  (-> (list-of-tt value-t) value-t)
  {match-lambda
    ['() value-null]
    [(cons x y) (cons-value-pair x (list->value y))]}}
{define (cons-value-list . xs) (list->value xs)}

{define/t sexp->value
  (-> any-t value-t)
  {match-lambda
    ['() value-null]
    [(? symbol? s) (cons-value-symbol (symbol->string s))]
    [(cons x y) (cons-value-pair (sexp->value x) (sexp->value y))]
    [(vector #f rest-part ...)
     {match rest-part
       [(list 'struct t v) (cons-value-struct (sexp->value t) (sexp->value v))]
       [(list 'delay exec-f display-f) (cons-value-delay exec-f display-f)]}]
    [(vector t xs ...) (cons-value-struct (sexp->value t) (sexp->value xs))]}}
{define/t (value-list-unjust-* x)
  (-> value-t (vector-tt (list-of-tt value-t) (or-tt nothing-t value-t)))
  {let ([x (value-unjust-* x)])
    {cond
      [(value-pair? x)
       {let ([(vector a b) (elim-value-pair x)])
         {let ([(vector head tail) (value-list-unjust-* b)])
           (vector (cons a head) tail)}}]
      [(value-null? x)
       (vector '() nothing)]
      [else (vector '() x)]}}}
{define/t (value->sexp x)
  (-> value-t any-t)
  {let ([x (value-unjust-* x)])
    {cond
      [(value-null? x) '()]
      [(value-symbol? x) (string->symbol (elim-value-symbol x))]
      [(value-pair? x)
       {let ([(vector a b) (elim-value-pair x)])
         (cons (value->sexp a) (value->sexp b))}]
      [(value-struct? x)
       {let ([(vector t xs) (elim-value-struct x)])
         {let ([(vector head tail) (value-list-unjust-* xs)])
           (if (nothing? tail)
               (list->vector (cons (value->sexp t) (map value->sexp head)))
               (vector #f 'struct t xs))}}]
      [(value-delay? x)
       {let ([(vector exec-f display-f) (elim-value-delay x)])
         (vector #f 'delay exec-f display-f)}]}}}

{define/t (unsafe-value-set-to-just! x v) ;; TODO: keep op
  (-> value-t value-t void-t)
  {when (not (or (point-eq? x v) (op/nat? x)))
    (vector-set*!
     x
     0 value-just-t-id
     1 v
     2 nothing
     3 nothing)}}
{define/t (value-unjust-* x)
  (-> value-t value-t)
  (value-unjust-*-aux x (set))}
{define/t (value-unjust-*-aux x history)
  (-> value-t (set-of-tt value-t) value-t)
  {cond
    [(set-member? history x) (TODO)]
    [(value-just? x) (value-unjust-*-aux (must-value-unjust-1 x) (set-add history x))]
    [else
     {for ([history_v history]) (unsafe-value-set-to-just! history_v x)}
     x]}}
{define/t (must-value-force-1 (and x (vector _ exec-f display-f _ ...)))
  (-> value-delay-t value-t)
  (vector-set! x 1 assert-unreachable)
  {let/t ([v value-t (exec-f)])
         (unsafe-value-set-to-just! x v)
         v}}
{define/t (value-force* x)
  (-> value-t value-t)
  (value-force*-aux x (set))}
{define/t (value-force*-aux x history)
  (-> value-t (set-of-tt value-t) value-t)
  {cond
    [(set-member? history x) (TODO)]
    [(value-just? x) (value-force*-aux (must-value-unjust-1 x) (set-add history x))]
    [(value-delay? x) (value-force*-aux (must-value-force-1 x) (set-add history x))]
    [else
     {for ([history_v history]) (unsafe-value-set-to-just! history_v x)}
     x]}}
{define (value-rec-force*-aux x (vector x0 x1) conser)
   {let ([x0 (value-force* x0)] [x1 (value-force* x1)])
     {let ([result (conser x0 x1)])
       (unsafe-value-set-to-just! x result)
       result}}}
{define/t (value-rec-force* x)
  (-> value-t value-t)
  {let ([x (value-force* x)])
    {cond
      [(or (value-null? x) (value-symbol? x)) x]
      [(value-pair? x) (value-rec-force*-aux x (elim-value-pair x) cons-value-pair)]
      [(value-struct? x) (value-rec-force*-aux x (elim-value-struct x) cons-value-struct)]}}}
{define/t ((value-undelay-m x display-f) f)
  (-> value-t (-> (vector-tt identifierspace-t value-t (list-of-tt value-t))) (cont-tt value-t value-t))
  {cond
    [(value-just? x) ((value-undelay-m (value-unjust-* x) display-f) f)]
    [(value-delay? x) (cons-value-delay {λ () ((value-undelay-m (must-value-force-1 x) display-f) f)} display-f)]
    [else (f x)]}}

{define/t (value-equal?-maker forcer)
  (-> (-> value-t value-t) (-> value-t value-t boolean-t))
  {define (value-equal?-made x y)
    (if (point-eq? x y)
        #t
        {let/t ([x value-t (forcer x)] [y value-t (forcer y)])
               {cond
                 [(point-eq? x y) #t]
                 [(or (value-delay? x) (value-delay? y)) #f]
                 [(value-null? x) {if (value-null? y)
                                      {begin
                                        (unsafe-value-set-to-just! x y)
                                        #t}
                                      #f}]
                 [(value-symbol? x) {if (and (value-symbol? y) (string-eq? (elim-value-symbol x) (elim-value-symbol y)))
                                        {begin
                                          (unsafe-value-set-to-just! x y)
                                          #t}
                                        #f}]
                 [(value-pair? x) {if (value-pair? y)
                                      {let ([(vector x0 x1) (elim-value-pair x)] [(vector y0 y1) (elim-value-pair y)])
                                        (and (value-equal?-made x0 y0)
                                             (value-equal?-made x1 y1))}
                                      #f}]
                 [(value-struct? x) {if (value-struct? y)
                                        {let ([(vector x0 x1) (elim-value-struct x)] [(vector y0 y1) (elim-value-struct y)])
                                          (and (value-equal?-made x0 y0)
                                               (value-equal?-made x1 y1))}
                                        #f}]}})}
  value-equal?-made}

{define/t value-equal? (-> value-t value-t boolean-t) (value-equal?-maker value-unjust-*)}
{define/t value-force+equal? (-> value-t value-t boolean-t) (value-equal?-maker value-force*)}

{define/t (value-undelay-list-or-return-m xs fail-v display-f)
  (-> value-t (-> value-t) (-> (vector-tt identifierspace-t value-t (list-of-tt value-t))) (cont-tt (list-of-tt value-t) value-t))
  {do cont->>=
    #{xs <- (value-undelay-m xs display-f)}
    {cond
      [(value-null? xs) (cont-return '())]
      [(value-pair? xs)
       {do cont->>=
         #{(vector xs-head xs-tail) := (elim-value-pair xs)}
         #{tail-r <- (value-undelay-list-or-return-m xs-tail fail-v display-f)}
         (cont-return (cons xs-head tail-r))}]
      [else {cont-if-return-m #t (fail-v)}]}}}

{define/t (value-undelay-list-m xs display-f)
  (-> value-t (-> (vector-tt identifierspace-t value-t (list-of-tt value-t))) (cont-tt (vector-tt (list-of-tt value-t) (or-tt nothing-t value-t)) value-t))
  {do cont->>=
    #{xs <- (value-undelay-m xs display-f)}
    {cond
      [(value-null? xs) (cont-return (vector '() nothing))]
      [(value-pair? xs)
       {do cont->>=
         #{(vector xs-head xs-tail) := (elim-value-pair xs)}
         #{(vector tail-r tail-r-tail) <- (value-undelay-list-m xs-tail display-f)}
         (cont-return (vector (cons xs-head tail-r) tail-r-tail))}]
      [else (cont-return (vector '() xs))]}}}

;; Influenced by: zh_CN, zh_TW, ja
{define/t exp-s value-symbol-t (cons-value-symbol "式")}
{define/t id-s value-symbol-t (cons-value-symbol "標識符")}
{define/t apply-function-s value-symbol-t (cons-value-symbol "用-函式")}
{define/t macro-s value-symbol-t (cons-value-symbol "構式子")}
{define/t quote-s value-symbol-t (cons-value-symbol "常量")}
{define/t function-s value-symbol-t (cons-value-symbol "函式")}
{define/t apply-macro-s value-symbol-t (cons-value-symbol "用-構式子")}
{define/t comment-s value-symbol-t (cons-value-symbol "注釋")}
{define/t error-s value-symbol-t (cons-value-symbol "異常")}
{define/t evaluate-s value-symbol-t (cons-value-symbol "解算")}
{define/t mapping-s value-symbol-t (cons-value-symbol "映射")}
{define/t builtin-s value-symbol-t (cons-value-symbol "內建")}
{define/t false-s value-symbol-t (cons-value-symbol "陰")}
{define/t true-s value-symbol-t (cons-value-symbol "陽")}
{define/t char-s value-symbol-t (cons-value-symbol "字符")}
{define/t string-s value-symbol-t (cons-value-symbol "字串")}
{define/t nat-zero-s value-symbol-t (cons-value-symbol "自然數/零")}
{define/t nat-succ-s value-symbol-t (cons-value-symbol "自然數/加一")}
{define/t symbol-to-string-s value-symbol-t (cons-value-symbol "符號→字串")}
{define/t string-to-symbol-s value-symbol-t (cons-value-symbol "字串→符號")}
{define/t cons-value-pair-s value-symbol-t (cons-value-symbol "構造-列表/序對")}
{define/t cons-value-struct-s value-symbol-t (cons-value-symbol "構造-結構體")}
{define/t elim-value-pair-s value-symbol-t (cons-value-symbol "解構-列表/序對")}
{define/t elim-value-struct-s value-symbol-t (cons-value-symbol "解構-結構體")}
{define/t value-pair?-s value-symbol-t (cons-value-symbol "列表/序對？")}
{define/t value-struct?-s value-symbol-t (cons-value-symbol "結構體？")}
{define/t value-null?-s value-symbol-t (cons-value-symbol "列表/空？")}
{define/t value-symbol?-s value-symbol-t (cons-value-symbol "符號？")}
{define/t rec-s value-symbol-t (cons-value-symbol "遞歸")}

{define/t false-v value-t (cons-value-struct false-s value-null)}
{define/t true-v value-t (cons-value-struct true-s value-null)}
{define/t (cons-value-boolean x)
  (-> boolean-t value-t)
  (if x true-v false-v)}

{define/t (identifierspace->value space)
  (-> identifierspace-t value-t)
  (cons-value-struct mapping-s (cons-value-list (list->value (map {λ ((vector k v)) (cons-value-list k v)} (identifierspace->list space)))))}
{define/t (value->identifierspace-or-return-m x ->error-v display-f)
  (-> value-t (-> value-t) (-> (vector-tt identifierspace-t value-t (list-of-tt value-t))) (cont-tt identifierspace-t value-t))
  {do cont->>=
    #{(list t) <- (elim-value-struct-fixed-type-and-length-or-return-m mapping-s 1 x ->error-v display-f)}
    #{xs <- (value-undelay-list-or-return-m t ->error-v display-f)}
    (value->identifierspace-or-return-m-aux identifierspace-null xs ->error-v display-f)}}
{define/t (value->identifierspace-or-return-m-aux r xs fail-v display-f)
  (-> identifierspace-t (list-of-tt value-t) (-> value-t) (-> (vector-tt identifierspace-t value-t (list-of-tt value-t))) (cont-tt identifierspace-t value-t))
  {if (null? xs)
      (cont-return r)
      {do cont->>=
        #{(cons head tail) := xs}
        #{head <- (value-undelay-list-or-return-m head fail-v display-f)}
        {cont-if-return-m (not (nat-eq? (length head) 2)) fail-v}
        #{(list k v) := head}
        (value->identifierspace-or-return-m-aux (identifierspace-set r k v) tail fail-v display-f)}}}

{define-match-expander value/
  {syntax-rules ()
    [(_ x) (? (curry value-equal? x))]}}

{define/t (elim-exp-comment-and-undelay-m x display-f)
  (-> value-t (-> (vector-tt identifierspace-t value-t (list-of-tt value-t))) (cont-tt value-t value-t))
  {do cont->>=
    #{x <- (value-undelay-m x display-f)}
    (if (not (value-struct? x))
        (cont-return x)
        {do cont->>=
          #{(vector x-type x-list) := (elim-value-struct x)}
          #{x-type <- (value-undelay-m x-type display-f)}
          (if (not (value-equal? x-type exp-s))
              (cont-return x)
              {do cont->>=
                #{(vector x-list x-list-tail) <- (value-undelay-list-m x-list display-f)}
                (if (not (and (nothing? x-list-tail) (nat-eq? (length x-list) 2)))
                    (cont-return x)
                    {do cont->>=
                      #{(list ast-type ast-list) := x-list}
                      #{ast-type <- (value-undelay-m ast-type display-f)}
                      (if (not (value-equal? ast-type comment-s))
                          (cont-return x)
                          {do cont->>=
                            #{(vector ast-list ast-list-tail) <- (value-undelay-list-m ast-list display-f)}
                            (if (not (and (nat-eq? (length ast-list 2)) (nothing? ast-list-tail)))
                                (cont-return x)
                                {let ([(list _ new-v) ast-list])
                                  (elim-exp-comment-and-undelay-m new-v display-f)})})})})})}}


{define/t (make-quote x)
  (-> value-t value-t)
  (cons-value-struct
   exp-s
   (cons-value-list
    builtin-s
    (cons-value-list
     quote-s
     x)))}
{define/t (cons-value-error t xs)
  (-> value-t value-t value-t)
  (cons-value-struct
   error-s
   (cons-value-list
    t
    xs))}
{define/t (cons-value-builtin-error space f xs)
  (-> identifierspace-t value-t (list-of-tt value-t) value-t)
  (cons-value-error
   builtin-s
   (cons-value-list f (list->value xs) (identifierspace->value space)))}
{define/t (list->exp xs)
  (-> (list-of-tt value-t) value-t)
  (if (null? xs)
      (make-quote value-null)
      (cons-value-struct
       exp-s
       (cons-value-list
        builtin-s
        (cons-value-list
         cons-value-pair-s
         (car xs)
         (list->exp (cdr xs))))))}
{define/t (elim-exp-or-return-m x ->error-v display-f)
  (->
   value-t
   (-> value-t)
   (-> (vector-tt identifierspace-t value-t (list-of-tt value-t)))
   (cont-tt (vector-tt value-t #|type|# (list-of-tt value-t) #|list|#) value-t))
  {do cont->>=
    #{(list ast-type ast-list) <- (elim-value-struct-fixed-type-and-length-or-return-m exp-s 2 x ->error-v display-f)}
    #{ast-list <- (value-undelay-list-or-return-m ast-list ->error-v display-f)}
    (cont-return (vector ast-type ast-list))}}
{define/t (elim-exp-id-or-return-m id ->error-v display-f)
  (->
   value-t
   (-> value-t)
   (-> (vector-tt identifierspace-t value-t (list-of-tt value-t)))
   (cont-tt value-t value-t))
  {do cont->>=
    #{(vector ast-type ast-list) <- (elim-exp-or-return-m id ->error-v display-f)}
    #{ast-type <- (elim-exp-comment-and-undelay-m ast-type display-f)}
    {match* (ast-type ast-list)
      [((value/ id-s) (list x)) (cont-return x)]
      [(_ _) (cont-return (DEBUG "elim-exp-id: not exp-id" (->error-v)))]}}}

{define/t (aux-evaluate-m ->error-v display-f space x)
  (->
   (-> value-t)
   (-> (vector-tt identifierspace-t value-t (list-of-tt value-t)))
   identifierspace-t
   value-t
   (cont-tt value-t value-t))
  {do cont->>=
    #{x <- (elim-exp-comment-and-undelay-m x display-f)}
    #{(vector ast-type ast-list) <- (elim-exp-or-return-m x ->error-v display-f)}
    #{ast-type <- (elim-exp-comment-and-undelay-m ast-type display-f)}
    #{ast-list <- (map-m {λ (v) (elim-exp-comment-and-undelay-m v display-f)} ast-list)}
    {match* (ast-type ast-list)
      [((value/ id-s) (list x))
       (cont-return (identifierspace-ref space x ->error-v))]
      [((value/ apply-function-s) (list f xs ...))
       (cont-return (eval-builtin space apply-function-s (list f (list->exp xs))))]
      [((value/ apply-macro-s) (list f xs ...))
       (cont-return (eval-builtin space apply-macro-s (list f (list->value xs))))]
      [((value/ builtin-s) (list f xs ...))
       (cont-return (eval-builtin space f xs))]
      [((value/ comment-s) (list comment x))
       ;; TODO: store comment for better error handling
       (aux-evaluate-m ->error-v display-f space x)]
      [(_ _)
       (cont-return (DEBUG "unknown exp"ast-type (->error-v)))]}}}
{define/t (function-arg-pat-match-or-return-m init-space pat vals ->error-v display-f)
  (->
   identifierspace-t
   value-t
   (list-of-tt value-t)
   (-> value-t)
   (-> (vector-tt identifierspace-t value-t (list-of-tt value-t)))
   (cont-tt identifierspace-t value-t))
  {do cont->>=
    #{(vector pat-head pat-tail) <- (value-undelay-list-m pat display-f)}
    #{pat-head <- (map-m {λ (v) (elim-exp-id-or-return-m v ->error-v display-f)} pat-head)}
    #{pat-tail <- (if (nothing? pat-tail)
                      (cont-return nothing)
                      (elim-exp-id-or-return-m pat-tail ->error-v display-f))}
    (function-arg-pat-match-or-return-m-aux init-space pat-head pat-tail vals ->error-v display-f)}}
{define/t (function-arg-pat-match-or-return-m-aux space ids id-tail vals ->error-v display-f)
  (->
   identifierspace-t
   (list-of-tt value-t)
   (or-tt value-t nothing-t)
   (list-of-tt value-t)
   (-> value-t)
   (-> (vector-tt identifierspace-t value-t (list-of-tt value-t)))
   (cont-tt identifierspace-t value-t))
  {match* (vals ids id-tail)
    [('() '() (? nothing?)) (cont-return space)]
    [(vals '() (? {λ (x) (not (nothing? x))} id-tail)) (cont-return (identifierspace-set space id-tail (list->value vals)))]
    [((cons v vals-tail) (cons ids-head ids-tail) id-tail)
     (function-arg-pat-match-or-return-m-aux
      (identifierspace-set space ids-head v)
      ids-tail
      id-tail
      vals-tail
      ->error-v
      display-f)]
    [(_ _ _) (cont-return (DEBUG "illegal arg pattern" (->error-v)))]}}

{define/t (aux-apply-m ->error-v display-f f xs)
  (->
   (-> value-t)
   (-> (vector-tt identifierspace-t value-t (list-of-tt value-t)))
   value-t
   (list-of-tt value-t)
   (cont-tt value-t value-t))
  {do cont->>=
    #{(list env arg-pat expr) <- (elim-value-struct-fixed-type-and-length-or-return-m function-s 3 f ->error-v display-f)}
    #{space <- (value->identifierspace-or-return-m env ->error-v display-f)}
    #{new-space <- (function-arg-pat-match-or-return-m space arg-pat xs ->error-v display-f)}
    (cont-return (evaluate new-space expr))}}
{define/t (list-append-value xs ys)
  (-> (list-of-tt value-t) value-t value-t)
  (if (null? xs)
      ys
      (cons-value-pair (car xs) (list-append-value (cdr xs) ys)))}
{define/t (aux-builtin-m space f raw-args)
  (->
   identifierspace-t
   value-t
   (list-of-tt value-t)
   (cont-tt value-t value-t))
  {define (display-f) (vector space f raw-args)}
  {define (->error-v) {match (display-f) [(vector s f xs) (cons-value-builtin-error s f xs)]}}
  {define (local-eval x)
    (evaluate space x)}
  {define/t (aux-cons-2 conser x-exp y-exp)
    (-> (-> value-t value-t value-t) value-t value-t (cont-tt value-t value-t))
    (cont-return (conser (local-eval x-exp) (local-eval y-exp)))}
  {define/t (aux-pred pred x-exp)
    (-> (-> value-t boolean-t) value-t (cont-tt value-t value-t))
    {do cont->>=
      #{x-val <- (value-undelay-m (local-eval x-exp) display-f)}
      (cont-return (cons-value-boolean (pred x-val)))}}
  {define (aux-elim-2 pred elimer x-exp id-a id-b r)
    (-> (-> value-t boolean-t) (-> value-t (vector-tt value-t value-t)) value-t value-t value-t (cont-tt value-t value-t))
    {do cont->>=
      #{id-a <- (elim-exp-comment-and-undelay-m id-a display-f)}
      #{id-b <- (elim-exp-comment-and-undelay-m id-b display-f)}
      #{id-a <- (elim-exp-id-or-return-m id-a ->error-v display-f)}
      #{id-b <- (elim-exp-id-or-return-m id-b ->error-v display-f)}
      #{x-val <- (value-undelay-m (local-eval x-exp) display-f)}
      {cont-if-return-m (not (pred x-val)) (DEBUG "elim: type error" (->error-v))}
      #{(vector a b) := (elimer x-val)}
      (cont-return (evaluate (identifierspace-set (identifierspace-set space id-a a) id-b b) r))}}
  {do cont->>=
    #{f <- (value-undelay-m f display-f)}
    {match* (f raw-args)
      [((value/ quote-s) (list v)) (cont-return v)]
      [((value/ evaluate-s) (list inner-space-exp x-exp))
       {do cont->>=
         #{x <- (local-eval x-exp)}
         #{inner-space-v <- (local-eval inner-space-exp)}
         #{inner-space <- (value->identifierspace-or-return-m inner-space-v ->error-v display-f)}
         (cont-return (aux-evaluate ->error-v display-f inner-space x))}]
      [((value/ function-s) (list args exp))
       {do cont->>=
         #{(vector args-head args-tail) <- (value-undelay-list-m args display-f)}
         #{args-head <- (map-m {λ (v) (elim-exp-comment-and-undelay-m v display-f)} args-head)}
         #{args-tail <- (if (nothing? args-tail) (cont-return value-null) (elim-exp-comment-and-undelay-m args-tail display-f))}
         (map-m {λ (v) (elim-exp-id-or-return-m v ->error-v display-f)} args-head) ;; check if it is exp/id
         (if (value-null? args-tail)
             (cont-return '())
             (elim-exp-id-or-return-m args-tail ->error-v display-f)) ;; check if it is exp/id
         (cont-return
          (cons-value-struct
           function-s
           (cons-value-list
            (identifierspace->value space)
            (list-append-value args-head args-tail)
            exp)))}]
      [((value/ apply-function-s) (list f xs))
       {do cont->>=
         #{xs <- (value-undelay-list-or-return-m (local-eval xs) ->error-v display-f)}
         (aux-apply-m
          ->error-v
          display-f
          (local-eval f)
          xs)}]
      [((value/ apply-macro-s) (list f xs))
       {do cont->>=
         #{xs <- (value-undelay-list-or-return-m (local-eval xs) ->error-v display-f)}
         (aux-apply-m
          ->error-v
          display-f
          (local-eval f)
          (cons-value-pair
           (identifierspace->value space)
           xs))}]
      [((value/ cons-value-struct-s) (list x y)) (aux-cons-2 cons-value-struct x y)]
      [((value/ cons-value-pair-s) (list x y)) (aux-cons-2 cons-value-pair x y)]
      [((value/ elim-value-struct-s) (list x id-a id-b r)) (aux-elim-2 value-struct? elim-value-struct x id-a id-b r)]
      [((value/ elim-value-pair-s) (list x id-a id-b r)) (aux-elim-2 value-pair? elim-value-pair x id-a id-b r)]
      [((value/ value-struct?-s) (list x)) (aux-pred value-struct? x)]
      [((value/ value-pair?-s) (list x)) (aux-pred value-pair? x)]
      [((value/ value-null?-s) (list x)) (aux-pred value-null? x)]
      [((value/ value-symbol?-s) (list x)) (aux-pred value-symbol? x)]
      [((value/ symbol-to-string-s) (list x))
       {do cont->>=
         #{x <- (value-undelay-m (local-eval x) ->error-v display-f)}
         {cont-if-return-m (not (value-symbol? x)) (DEBUG "symbol-to-string: type error" (->error-v))}
         {cont-return (string->value (elim-value-symbol x))}}]
      [((value/ string-to-symbol-s) (list x))
       {do cont->>=
         #{str <- (value->string-or-return-m (local-eval x) ->error-v display-f)}
         (cont-return (cons-value-symbol str))}]
      [((value/ rec-s) (list id expr))
       {do cont->>=
         #{id <- (elim-exp-comment-and-undelay-m id display-f)}
         #{id <- (elim-exp-id-or-return-m id ->error-v display-f)}
         (cont-return
          {letrec
              ([val (evaluate:syntax inner-space expr)]
               [inner-space (identifierspace-set space id val)])
            val})}]
      [(_ _) (cont-return (DEBUG "unknown builtin" (->error-v)))]}}}

{define-syntax-rule (aux-evaluate:syntax ->error-v display-f space x)
  (cons-value-delay {λ () ((aux-evaluate-m ->error-v display-f space x) id)} display-f)}
{define/t (aux-evaluate ->error-v display-f space x)
  (->
   (-> value-t)
   (-> (vector-tt identifierspace-t value-t (list-of-tt value-t)))
   identifierspace-t
   value-t
   value-t)
  (aux-evaluate:syntax ->error-v display-f space x)}
{define-syntax-rule (evaluate:syntax space x)
  ({λ ()
     {define (display-f)
       (vector
        identifierspace-null
        evaluate-s
        (list
         (make-quote (identifierspace->value space))
         (make-quote x)))}
     {define (->error-v) {match (display-f) [(vector s f xs) (cons-value-builtin-error s f xs)]}}
     (aux-evaluate ->error-v display-f space x)})}
{define/t (evaluate space x)
  (-> identifierspace-t value-t value-t)
  (evaluate:syntax space x)}
{define/t (eval-builtin space f xs)
  (-> identifierspace-t value-t (list-of-tt value-t) value-t)
  (cons-value-delay {λ () ((aux-builtin-m space f xs) id)} {λ () (vector space f xs)})}
{define/t (aux-eval-apply ->error-v display-f f xs)
  (->
   (-> value-t)
   (-> (vector-tt identifierspace-t value-t (list-of-tt value-t)))
   value-t
   (list-of-tt value-t)
   value-t)
  (cons-value-delay {λ () ((aux-apply-m ->error-v display-f f xs) id)} display-f)}
{define/t (eval-apply f xs)
  (-> value-t (list-of-tt value-t) value-t)
  {define (display-f)
    (vector
     identifierspace-null
     apply-function-s
     (list
      (make-quote f)
      (make-quote (list->value xs))))}
  {define (->error-v) {match (display-f) [(vector s f xs) (cons-value-builtin-error s f xs)]}}
  (aux-eval-apply ->error-v display-f f xs)}

{define (unittest)
  {local-require rackunit}
  {let
      ([tests
        `((,(identifierspace-set
             identifierspace-null
             (sexp->value 'x)
             (sexp->value 'v)) #(式 標識符 (x)) v)
          (,identifierspace-null #(式 內建 (常量 x)) x)
          (,identifierspace-null #(式 用-函式 (#(式 內建 (函式 (#(式 標識符 (x))) #(式 標識符 (x)))) #(式 內建 (常量 v)))) v)
          (,identifierspace-null #(式 用-函式 (#(式 內建 (函式 #(式 標識符 (x)) #(式 標識符 (x)))) #(式 內建 (常量 v)))) (v))
          (,identifierspace-null #(式 用-函式 (#(式 內建 (函式 (#(式 標識符 (x)) . #(式 標識符 (y))) #(式 標識符 (y)))) #(式 內建 (常量 v)))) ())
          )])
    {for
        ([test tests])
      {let ([(list space exp val) test])
        (check value-equal? (sexp->value val) (value-rec-force* (evaluate space (sexp->value exp))))}}}}
