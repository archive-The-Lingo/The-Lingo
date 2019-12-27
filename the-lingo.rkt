#|
    Copyright (C) 2019  ㄗㄠˋ ㄑㄧˊ <tsao-chi@the-lingo.org>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published
    by the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#
#lang racket

;; This file is written in a Racket dialect defined and described below
{define-syntax-rule {if-typecheck-on t f} t}
#|
  cons-    constructor
  elim-    eliminator
  eq    equal
  nat    natural numbers
  exp    expression
  { .. }    macro/syntax
  /t -t t    type
  -tt    a function returning types
  -*    all
  !    read/write mutable values
  :    type annotation
  -m    monad/async/cps
  -aux    auxiliary
  -s    symbol
  -f    function
  -v    value
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
{define:type null-t null?}
{define:type symbol-t symbol?}
{define-syntax-rule {list-foreach xs v . c} {for ([v xs]) . c}}
{define:type nothing-t void-t}
{define/t nothing nothing-t (void)}
{define/t (nothing? x)
  (-> any-t boolean-t)
  (equal? x nothing)}
{define (assert-unreachable) (error 'assert-unreachable)}
{define (TODO) (error 'WIP)}
{define (id x) x}
{define point-eq? eq?}
{define nat-eq? =}
{define string-eq? string=?}
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
    (-> (vector-tt identifierspace-t value-t)) ;; display-f
    nothing-t))}
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

{define/t (value-symbol? (vector t _ ...))
  (-> value-t boolean-t)
  (t-id-eq? t value-symbol-t-id)}
{define/t (value-pair? (vector t _ ...))
  (-> value-t boolean-t)
  (t-id-eq? t value-pair-t-id)}
{define/t (value-null? (vector t _ ...))
  (-> value-t boolean-t)
  (t-id-eq? t value-null-t-id)}
{define/t (value-struct? (vector t _ ...))
  (-> value-t boolean-t)
  (t-id-eq? t value-struct-t-id)}
{define/t (value-just? (vector t _ ...))
  (-> value-t boolean-t)
  (t-id-eq? t value-just-t-id)}
{define/t (value-delay? (vector t _ ...))
  (-> value-t boolean-t)
  (t-id-eq? t value-delay-t-id)}

{define/t (cons-value-symbol x)
  (-> string-t value-symbol-t)
  (vector value-symbol-t-id x nothing nothing)}
{define/t (elim-value-symbol (vector _ v _ ...))
  (-> value-symbol-t string-t)
  v}
{define/t (value-symbol-equal? x y)
  (-> value-symbol-t value-symbol-t boolean-t)
  (string-eq? (elim-value-symbol x) (elim-value-symbol y))}
{define/t (cons-value-pair x y)
  (-> value-t value-t value-pair-t)
  (vector value-pair-t-id x y nothing)}
{define/t (elim-value-pair (vector _ x y _ ...))
  (-> value-pair-t (vector-tt value-t value-t))
  (vector x y)}
{define/t (cons-value-struct x y)
  (-> value-t value-t value-struct-t)
  (vector value-struct-t-id x y nothing)}
{define/t (elim-value-struct (vector _ x y _ ...))
  (-> value-struct-t (vector-tt value-t value-t))
  (vector x y)}
{define/t value-null value-null-t (vector value-null-t-id nothing nothing nothing)}
{define/t (cons-value-delay exec-f display-f)
  (-> (-> value-t) (-> (vector-tt identifierspace-t value-t)) value-delay-t)
  (vector value-delay-t-id exec-f display-f nothing)}
{define/t (elim-value-delay (vector _ exec-f display-f _ ...))
  (-> value-delay-t (vector-tt (-> value-t) (-> (vector-tt identifierspace-t value-t))))
  (vector exec-f display-f)}

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

{define/t (unsafe--value-set-to-just! x v)
  (-> value-t value-t void-t)
  {when (not (point-eq? x v))
    (vector-set*! x
                  0 value-just-t-id
                  1 v
                  2 nothing
                  3 nothing)}}
{define/t (must-value-unjust-1 (vector _ v _ ...))
  (-> value-just-t value-t)
  v}
{define/t (value-unjust-* x)
  (-> value-t value-t)
  (value-unjust-*-aux x (list x))}
{define/t (value-unjust-*-aux x history)
  (-> value-t (list-of-tt value-t) value-t)
  (if (value-just? x) (value-unjust-*-aux (must-value-unjust-1 x) (cons x history))
      {begin
        {list-foreach history history_v (unsafe--value-set-to-just! history_v x)}
        x})}
{define/t (must-value-force-1 (and x (vector _ exec-f display-f _ ...)))
  (-> value-delay-t value-t)
  (vector-set! x 1 assert-unreachable)
  {let/t ([v value-t (exec-f)])
         (unsafe--value-set-to-just! x v)
         v}}
{define/t (value-force* x)
  (-> value-t value-t)
  (value-force*-aux x (list x))}
{define/t (value-force*-aux x history)
  (-> value-t (list-of-tt value-t) value-t)
  {cond
    [(value-just? x) (value-force*-aux (must-value-unjust-1 x) (cons x history))]
    [(value-delay? x) (value-force*-aux (must-value-force-1 x) (cons x history))]
    [else
     {list-foreach history history_v (unsafe--value-set-to-just! history_v x)}
     x]}}
{define:type (cont-tt a r) (-> (-> a r) r)}
{define/t (cont-return x)
  (-> any-t (cont-tt any-t any-t))
  {λ (c) (c x)}}
{define/t (cont->>= x f)
  (-> (cont-tt any-t any-t) (-> any-t (cont-tt any-t any-t)) (cont-tt any-t any-t))
  {λ (c) (x {λ (v) ((f v) c)})}}
{define-syntax-rule {cont-if-return-m b v}
  {λ (c) (if b v (c nothing))}}
{define/t ((value-undelay-m x display-f) f)
  (-> value-t (-> (vector-tt identifierspace-t value-t)) (cont-tt value-t value-t))
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
                                        (unsafe--value-set-to-just! x y)
                                        #t}
                                      #f}]
                 [(value-symbol? x) {if (and (value-symbol? y) (string-eq? (elim-value-symbol x) (elim-value-symbol y)))
                                        {begin
                                          (unsafe--value-set-to-just! x y)
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
  (-> value-t (-> value-t) (-> (vector-tt identifierspace-t value-t)) (cont-tt (list-of-tt value-t) value-t))
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
  (-> value-t (-> (vector-tt identifierspace-t value-t)) (cont-tt (vector-tt (list-of-tt value-t) (or-tt nothing-t value-t)) value-t))
  {do cont->>=
    #{xs <- (value-undelay-m xs display-f)}
    {cond
      [(value-null? xs) (cont-return (vector '() nothing))]
      [(value-pair? xs)
       {do cont->>=
         #{(vector xs-head xs-tail) := (elim-value-pair xs)}
         #{(vector tail-r tail-r--tail) <- (value-undelay-list-m xs-tail display-f)}
         (cont-return (vector (cons xs-head tail-r) tail-r--tail))}]
      [else (cont-return (vector '() xs))]}}}

;; Influenced by: zh_CN, zh_TW, ja
{define/t exp-s value-symbol-t (cons-value-symbol "式")}
{define/t id-s value-symbol-t (cons-value-symbol "標符")}
{define/t apply-function-s value-symbol-t (cons-value-symbol "用-函式")}
{define/t macro-s value-symbol-t (cons-value-symbol "構式子")}
{define/t quote-s value-symbol-t (cons-value-symbol "引用")}
{define/t function-s value-symbol-t (cons-value-symbol "函式")}
{define/t apply-macro-s value-symbol-t (cons-value-symbol "用-構式子")}
{define/t comment-s value-symbol-t (cons-value-symbol "注釋")}
{define/t error-s value-symbol-t (cons-value-symbol "異常")}
{define/t eval-s value-symbol-t (cons-value-symbol "解算")}
{define/t mapping-s value-symbol-t (cons-value-symbol "映射")}
{define/t builtin-s value-symbol-t (cons-value-symbol "內建")}

{define/t (identifierspace->value space)
  (-> identifierspace-t value-t)
  (cons-value-struct mapping-s (cons-value-list (list->value (map {λ ((vector k v)) (cons-value-list k v)} (identifierspace->list space)))))}
{define/t (value->identifierspace-or-return-m x fail-v display-f)
  (-> value-t (-> value-t) (-> (vector-tt identifierspace-t value-t)) (cont-tt identifierspace-t value-t))
  {do cont->>=
    #{x <- (value-undelay-m x display-f)}
    {cont-if-return-m (not (value-struct? x)) fail-v}
    #{(vector x-type x-list) := (elim-value-struct x)}
    #{x-type <- (value-undelay-m x-type display-f)}
    {cont-if-return-m (not (value-equal? x-type mapping-s)) fail-v}
    #{x-list <- (value-undelay-list-or-return-m x-list fail-v display-f)}
    {cont-if-return-m (not (nat-eq? (length x-list) 1)) fail-v}
    #{(list t) := x-list}
    #{xs <- (value-undelay-list-or-return-m t fail-v display-f)}
    (value->identifierspace-or-return-m-aux identifierspace-null xs fail-v display-f)}}
{define/t (value->identifierspace-or-return-m-aux r xs fail-v display-f)
  (-> identifierspace-t (list-of-tt value-t) (-> value-t) (-> (vector-tt identifierspace-t value-t)) (cont-tt identifierspace-t value-t))
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

{define/t (elim-exp-comment-m x display-f)
  (-> value-t (-> (vector-tt identifierspace-t value-t)) (cont-tt value-t value-t))
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
                #{(vector x-list x-list--tail) <- (value-undelay-list-m x-list display-f)}
                (if (not (and (nothing? x-list--tail) (nat-eq? (length x-list) 2)))
                    (cont-return x)
                    {do cont->>=
                      #{(list ast-type ast-list) := x-list}
                      #{ast-type <- (value-undelay-m ast-type display-f)}
                      (if (not (value-equal? ast-type comment-s))
                          (cont-return x)
                          {do cont->>=
                            #{(vector ast-list ast-list--tail) <- (value-undelay-list-m ast-list display-f)}
                            (if (not (and (nat-eq? (length ast-list 2)) (nothing? ast-list--tail)))
                                (cont-return x)
                                {let ([(list _ new-v) ast-list])
                                  (elim-exp-comment-m new-v display-f)})})})})})}}

{define/t (evaluate space x)
  (-> identifierspace-t value-t value-t)
  (cons-value-delay {λ () ((evaluate-aux space x) id)} {λ () (vector space x)})}
{define/t (make-quote x)
  (-> value-t value-t)
  (cons-value-struct
   exp-s
   (cons-value-list
    builtin-s
    (cons-value-list
     quote-s
     x)))}
{define/t (evaluate-aux space x)
  (-> identifierspace-t value-t (cont-tt value-t value-t))
  {define (display-f) (vector space x)}
  {define (->error-v)
    (cons-value-struct
     error-s
     (cons-value-list
      (cons-value-list builtin-s eval-s)
      (cons-value-list
       (identifierspace->value space)
       x)))}
  {do cont->>=
    #{x <- (value-undelay-m x display-f)}
    {cont-if-return-m (not (value-struct? x)) (->error-v)}
    #{(vector x-type x-list) := (elim-value-struct x)}
    #{x-type <- (value-undelay-m x-type display-f)}
    {cont-if-return-m (not (value-equal? x-type exp-s)) (->error-v)}
    #{x-list <- (value-undelay-list-or-return-m x-list ->error-v display-f)}
    {cont-if-return-m (not (nat-eq? (length x-list) 2)) (->error-v)}
    #{(list ast-type ast-list) := x-list}
    #{ast-type <- (value-undelay-m ast-type display-f)}
    #{ast-list <- (value-undelay-list-or-return-m ast-list ->error-v display-f)}
    {match* (ast-type ast-list)
      [((value/ id-s) (list x))
       (cont-return (identifierspace-ref space x ->error-v))]
      [((value/ apply-function-s) (list f xs ...))
       (cont-return (value-apply (evaluate space f) (map (curry evaluate space) xs)))]
      [((value/ apply-macro-s) (list f xs ...))
       {do cont->>=
         #{f <- (value-undelay-m (evaluate space f) display-f)}
         {cont-if-return-m (not (value-struct? x)) (->error-v)}
         #{(vector f-type f-list) := (elim-value-struct f)}
         #{f-type <- (value-undelay-m f-type display-f)}
         (TODO)}]
      [((value/ builtin-s) (list f xs ...))
       {do cont->>=
         #{f <- (value-undelay-m f display-f)}
         {match* (f xs)
           [((value/ quote-s) (list v)) v]
           [((value/ eval-s) (list space x))
            {do cont->>=
               #{s <- (value->identifierspace-or-return-m space TODO #|->error-v for builtin|# display-f)}
               (cont-return (evaluate s x))}]
           [((value/ apply-function-s) (list f xs))
            {do cont->>=
               #{xs <- (value-undelay-list-or-return-m xs TODO #|->error-v for builtin|# display-f)}
               (cont-return (value-apply f xs))}]
           [((value/ function-s) (list arg-id expr))
            {do cont->>=
               #{arg-id <- (elim-exp-comment-m arg-id display-f)}
               #{arg-id <- (value-undelay-m arg-id display-f)}
               {cont-if-return-m (not (value-struct? arg-id)) (->error-v)}
               #{upvals := (filter
                            {λ ((vector _ d)) (not (nothing? d))}
                            (identifierspace->list (identifierspace-set space arg-id nothing)))}
               (if (null? upvals)
                   (cons-value-struct
                    function-s
                    (cons-value-list
                     arg-id
                     expr))
                   (cons-value-struct
                    function-s
                    (cons-value-list
                     arg-id
                     ((TODO #| cons a identifierspace and evaluate expr |#) expr))))}]
           [(_ _) (cont-return (->error-v))]}}]
      [((value/ comment-s) (list comment x))
       ;; todo: store comment for better error handling
       (evaluate-aux space x)]
      [(_ _)
       (cont-return (->error-v))]}}}
{define/t (value-apply f xs)
  (-> value-t (list-of-tt value-t) value-t)
  ((value-apply-aux f xs) id)}
{define/t (value-apply-aux f xs)
  (-> value-t (list-of-tt value-t) (cont-tt value-t value-t))
  {define (display-f)
    (vector
     identifierspace-null
     (cons-value-struct
      exp-s
      (cons-value-list
       apply-function-s
       (cons-value-pair
        (make-quote f)
        (list->value (map make-quote xs))))))}
  {define (->error-v)
    (cons-value-struct
     error-s
     (cons-value-list
      (cons-value-list builtin-s apply-function-s)
      (cons-value-list
       f
       (list->value xs))))}
  {do cont->>=
    #{f <- (value-undelay-m f display-f)}
    {cont-if-return-m (not (value-struct? f)) (->error-v)}
    #{(vector f-type f-list) <- (elim-value-struct f)}
    #{f-type <- (value-undelay-m f-type display-f)}
    {cont-if-return-m (not (value-equal? f-type function-s)) (->error-v)}
    #{f-list <- (value-undelay-list-or-return-m f-list ->error-v display-f)}
    {cont-if-return-m (not (nat-eq? (length f-list) 2)) (->error-v)}
    #{(list arg-id expr) := f-list}
    (cont-return (evaluate (identifierspace-set identifierspace-null arg-id (list->value xs)) expr))}}

{define (unittest)
  {local-require rackunit}
  (check
   value-equal?
   (sexp->value 'v)
   (value-force*
    (evaluate
     (identifierspace-set
      identifierspace-null
      (sexp->value 'x)
      (sexp->value 'v))
     (sexp->value '#(式 標符 (x))))))}
