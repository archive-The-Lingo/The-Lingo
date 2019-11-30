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
#|
  create-    create mutable values
  cons-    constructor
  elim-    eliminator
  { .. }    macro/syntax
  /t -t    type
  -tt    a function returning types
  -*    all
  !    read/write mutable values
  :    type annotation
  -m    monad/async/cps
  -aux    auxiliary
  -s    symbol
|#
{require racket/contract}
{require (only-in typed/racket assert)}
{define-syntax-rule {if-typecheck-on t f} t}
{define-syntax-rule {define:type . xs} {define . xs}}
{define-syntax-rule {define-syntax-rule:type . xs} {define-syntax-rule . xs}}
{if-typecheck-on
 {define-syntax-rule {define/t . xs} {define/contract . xs}}
 {define-syntax-rule {define/t n t . xs} {define n . xs}}}
{if-typecheck-on
 {define-syntax-rule {let/t ([id typ val] ...) . r}
   {let ([id ({λ () {define/t tmp typ val} tmp})] ...) . r}}
 {define-syntax-rule {let/t ([id typ val] ...) . r}
   {let ([id val] ...) . r}}}
{define-syntax-rule (t->? t)
  {let ([t-lazy (delay t)])
    {λ (x)
      {with-handlers ([exn:fail:contract? {λ (e) #f}])
        {define/contract _ (force t-lazy) x}
        #t}}}}
{define-syntax-rule (rec-type x) (recursive-contract x #:chaperone)}
{define:type and-tt and/c}
{define:type or-tt or/c}
{define:type not-tt not/c}
{define:type any-t any/c}
{define:type string-t string?}
{define:type char-t char?}
{define:type vector-tt vector/c}
{define:type void-t void?}
{define:type boolean-t boolean?}
{define:type box-t box/c}
{define:type list-of-tt listof}
{define:type null-t null?}
{define-syntax-rule {list-foreach xs v . c} {for ([v xs]) . c}}
{define:type nothing-t void-t}
{define/t nothing nothing-t (void)}
{define (assert-unreachable) (error 'assert-unreachable)}
{define (WIP) (error 'WIP)}
{define (id x) x}

{define-syntax do
  {syntax-rules (<- :=)
    [(_ >>= x) x]
    [(_ >>= {:= x v} . r) {let ([x v]) {do >>= . r}}]
    [(_ >>= {<- x v} . r) (>>= v {λ (x) {do >>= . r}})]
    [(_ >>= v . r) {do >>= {<- x v} . r}]}}

{define:type t-id-t natural-number/c}
{define:type value-bone-t (vector-tt t-id-t any-t any-t any-t)}

{define-values (identifierspace? identifierspace-null identifierspace-ref identifierspace-set)
  ({λ ()
     {define (val-eq? x y) (value-force+equal? x y)} ;; because "cannot reference an identifier before its definition"
     {define-custom-hash-types identifierspace
       #:key? (t->? value-t)
       val-eq?}
     {define identifierspace-null (make-immutable-identifierspace '())}
     {define identifierspace-ref dict-ref}
     {define identifierspace-set dict-set}
     (values immutable-identifierspace? identifierspace-null identifierspace-ref identifierspace-set)})}
{define:type identifierspace-t identifierspace?}

{define:type value-symbol-t-id-t (and-tt t-id-t 0)}
{define/t value-symbol-t-id value-symbol-t-id-t 0}
{define:type value-pair-t-id-t (and-tt t-id-t 1)}
{define/t value-pair-t-id value-pair-t-id-t 1}
{define:type value-null-t-id-t (and-tt t-id-t 2)}
{define/t value-null-t-id value-null-t-id-t 2}
{define:type value-struct-t-id-t (and-tt t-id-t 3)}
{define/t value-struct-t-id value-struct-t-id-t 3}
{define:type value-char-t-id-t (and-tt t-id-t 4)}
{define/t value-char-t-id value-char-t-id-t 4}
{define:type value-just-t-id-t (and-tt t-id-t 5)}
{define/t value-just-t-id value-just-t-id-t 5}
{define:type value-delay-t-id-t (and-tt t-id-t 6)}
{define/t value-delay-t-id value-delay-t-id-t 6}
{define:type value-comment-t-id-t (and-tt t-id-t 7)}
{define/t value-comment-t-id value-comment-t-id-t 7}

{define-syntax-rule:type (value-tt x) (t->? (and-tt value-bone-t x))} ;; without t->?, it will make the same value different and disallow changing the type of value
{define:type value-symbol-t (value-tt (vector-tt value-symbol-t-id-t string-t nothing-t nothing-t))}
{define:type value-pair-t (value-tt (vector-tt value-pair-t-id-t value-t value-t nothing-t))}
{define:type value-null-t (value-tt (vector-tt value-null-t-id-t nothing-t nothing-t nothing-t))}
{define:type value-struct-t (value-tt (vector-tt value-struct-t-id-t value-t value-t nothing-t))}
{define:type value-char-t (value-tt (vector-tt value-char-t-id-t char-t nothing-t nothing-t))}
{define:type value-just-t (value-tt (vector-tt value-just-t-id-t value-t nothing-t nothing-t))}
{define:type value-delay-t (value-tt (vector-tt value-delay-t-id-t (-> value-t) (-> (vector-tt identifierspace-t value-t)) nothing-t))} ;; exec:`(-> value-t)`/display:`(-> (vector-t identifierspace-t value-t))`
{define:type value-comment-t (value-tt (vector-tt value-comment-t-id-t value-t (list-of-tt value-t) nothing-t))} ;; val:value-t/comment:`(arrayof-t value-t)`
{define:type value-t
  (t->?
   (or-tt
    value-symbol-t
    value-pair-t
    value-null-t
    value-struct-t
    value-char-t
    value-just-t
    value-delay-t
    value-comment-t
    ))}

{define/t (value-symbol? x)
  (-> value-t boolean-t)
  (= (vector-ref x 0) value-symbol-t-id)}
{define/t (value-pair? x)
  (-> value-t boolean-t)
  (= (vector-ref x 0) value-pair-t-id)}
{define/t (value-null? x)
  (-> value-t boolean-t)
  (= (vector-ref x 0) value-null-t-id)}
{define/t (value-struct? x)
  (-> value-t boolean-t)
  (= (vector-ref x 0) value-struct-t-id)}
{define/t (value-char? x)
  (-> value-t boolean-t)
  (= (vector-ref x 0) value-char-t-id)}
{define/t (value-just? x)
  (-> value-t boolean-t)
  (= (vector-ref x 0) value-just-t-id)}
{define/t (value-delay? x)
  (-> value-t boolean-t)
  (= (vector-ref x 0) value-delay-t-id)}
{define/t (value-comment? x)
  (-> value-t boolean-t)
  (= (vector-ref x 0) value-comment-t-id)}

{define/t (cons-value-symbol x)
  (-> string-t value-symbol-t)
  (vector value-symbol-t-id x nothing nothing)}
{define/t (elim-value-symbol x)
  (-> value-symbol-t string-t)
  (vector-ref x 1)}
{define/t (cons-value-pair x y)
  (-> value-t value-t value-pair-t)
  (vector value-pair-t-id x y nothing)}
{define/t (elim-value-pair x)
  (-> value-pair-t (vector-tt value-t value-t))
  (vector (vector-ref x 1) (vector-ref x 2))}
{define/t (cons-value-struct x y)
  (-> value-t value-t value-struct-t)
  (vector value-struct-t-id x y nothing)}
{define/t (elim-value-struct x)
  (-> value-struct-t (vector-tt value-t value-t))
  (vector (vector-ref x 1) (vector-ref x 2))}
{define/t value-null value-null-t (vector value-null-t-id nothing nothing nothing)}
{define/t (cons-value-char x)
  (-> char-t value-char-t)
  (vector value-char-t-id x nothing nothing)}
{define/t (elim-value-char x)
  (-> value-char-t char-t)
  (vector-ref x 1)}
{define/t (must-cons-value-comment x comment)
  (-> value-t (and-tt (not-tt null-t) (list-of-tt value-t)) value-comment-t)
  (vector value-comment-t-id x comment nothing)}
{define/t (cons-value-comment x comment)
  (-> value-t (list-of-tt value-t) value-t)
  (if (null? comment) x (must-cons-value-comment x comment))}
{define/t (elim-value-comment x)
  (-> value-comment-t (vector-tt value-t (list-of-tt value-t)))
  (vector (vector-ref x 1) (vector-ref x 2))}
{define/t (cons-value-delay exec display_f)
  (-> (-> value-t) (-> (vector-tt identifierspace-t value-t)) value-delay-t)
  (vector value-delay-t-id exec display_f nothing)}

{define/t (elim-value-comment-* x)
  (-> value-comment-t (vector-tt value-t (list-of-tt value-t)))
  (elim-value-comment-*-aux x (list x) (list))}
{define/t (elim-value-comment-*-aux x history comments)
  (-> value-t (list-of-tt value-t) (list-of-tt value-t) (vector-tt value-t (list-of-tt value-t)))
  {cond
    [(value-just? x) (elim-value-comment-*-aux (value-unjust-* x) (cons history x) comments)]
    [(value-comment? x) {let ([x01 (elim-value-comment x)])
                          (elim-value-comment-*-aux
                           (vector-ref x 0)
                           (cons history x)
                           (append comments (vector-ref x 1)))}]
    [else
     {list-foreach history history_v (value-unsafe-set-to-just! history_v x)}
     (vector x comments)]}}
{define/t (value-unsafe-set-to-just! x v)
  (-> value-t value-t void-t)
  {when (not (eq? x v))
    (vector-set*! x
                  0 value-just-t-id
                  1 v
                  2 nothing
                  3 nothing)}}
{define/t (must-value-unjust-1 x)
  (-> value-just-t value-t)
  (vector-ref x 1)}
{define/t (value-unjust-* x)
  (-> value-t value-t)
  (value-unjust-*-aux x (list x))}
{define/t (value-unjust-*-aux x history)
  (-> value-t (list-of-tt value-t) value-t)
  (if (value-just? x) (value-unjust-*-aux (must-value-unjust-1 x) (cons history x))
      {begin
        {list-foreach history history_v (value-unsafe-set-to-just! history_v x)}
        x})}
{define/t (must-nocache-value-force-1 x)
  (-> value-delay-t value-t)
  {let/t ([exec (-> value-t) (vector-ref x 1)])
         {let/t ([v value-t (exec)])
                v}}}
{define/t (must-value-force-1 x)
  (-> value-delay-t value-t)
  {let/t ([v value-t (must-nocache-value-force-1 x)])
         (value-unsafe-set-to-just! x v)
         v}}
{define/t (value-force x)
  (-> value-t value-t)
  (value-force-aux x (list x))}
{define/t (value-force-aux x history)
  (-> value-t (list-of-tt value-t) value-t)
  {cond
    [(value-just? x) (value-force-aux (must-value-unjust-1 x) (cons history x))]
    [(value-delay? x) (value-force-aux (must-value-force-1 x) (cons history x))]
    [else
     {list-foreach history history_v (value-unsafe-set-to-just! history_v x)}
     x]}}
{define:type (cont-tt a r) (-> (-> a r) r)}
{define/t (cont-return x)
  (-> any-t (cont-tt any-t any-t))
  {λ (c) (c x)}}
{define/t (cont->>= x f)
  (-> (cont-tt any-t any-t) (-> any-t (cont-tt any-t any-t)) (cont-tt any-t any-t))
  {λ (c) (x {λ (v) ((f v) c)})}}
{define/t ((value-undelay-m x display_f) f)
  (-> value-t (-> (vector-tt identifierspace-t value-t)) (cont-tt (vector-tt value-t (list-of-tt value-t)) value-t))
  (value-undelay-m-aux x display_f (list) f)}
{define/t (value-undelay-m-aux x display_f comments f)
  (-> value-t (-> (vector-tt identifierspace-t value-t)) (list-of-tt value-t) (-> (vector-tt value-t (list-of-tt value-t)) value-t) value-t)
  {cond
    [(value-comment? x)
     {let/t ([x01 (vector-tt value-t (list-of-tt value-t)) (elim-value-comment-* x)])
            {let/t ([new-v value-t (vector-ref x01 0)] [new-comments (list-of-tt value-t) (vector-ref x01 1)])
                   (value-undelay-m-aux new-v (append comments new-comments) f)}}]
    [(value-just? x) (value-undelay-m-aux (value-unjust-* x) display_f comments f)]
    [(value-delay? x) (cons-value-delay {λ () (value-undelay-m-aux (must-value-force-1 x) display_f comments f)} display_f)]
    [else (f (vector x comments))]}}

{define/t (value-force+equal? x y)
  (-> value-t value-t boolean-t)
  (value-force+equal?-aux x y (box #f))}
{define/t (value-force+equal?-aux x y inner-equal-and-has-comment)
  (-> value-t value-t (box-t boolean-t) boolean-t)
  (if (eq? x y)
      #t
      {let/t ([x value-t (value-force x)] [y value-t (value-force y)])
             {cond
               [(eq? x y) #t]
               [(value-comment? x)
                {let/t ([x01 (vector-tt value-t (list-of-tt value-t)) (elim-value-comment-* x)])
                       (set-box! inner-equal-and-has-comment #t)
                       (value-force+equal?-aux (vector-ref x 0) y inner-equal-and-has-comment)}]
               [(value-comment? y) (value-force+equal?-aux y x inner-equal-and-has-comment)]
               [(value-null? x) {if (value-null? y)
                                    {begin
                                      (value-unsafe-set-to-just! x y)
                                      #t}
                                    #f}]
               [(value-symbol? x) {if (and (value-symbol? y) (string=? (elim-value-symbol x) (elim-value-symbol y)))
                                      {begin
                                        (value-unsafe-set-to-just! x y)
                                        #t}
                                      #f}]
               [(value-char? x) {if (and (value-char? y) (char=? (elim-value-char x) (elim-value-char y)))
                                    {begin
                                      (value-unsafe-set-to-just! x y)
                                      #t}
                                    #f}]
               [(value-pair? x) {if (value-pair? y)
                                    {let ([x01 (elim-value-pair x)] [y01 (elim-value-pair y)])
                                      (value-force+equal?-aux-aux-pair
                                       x (vector-ref x01 0) (vector-ref x01 1)
                                       y (vector-ref y01 0) (vector-ref y01 1)
                                       inner-equal-and-has-comment)}
                                    #f}]
               [(value-struct? x) {if (value-struct? y)
                                    {let ([x01 (elim-value-struct x)] [y01 (elim-value-struct y)])
                                      (value-force+equal?-aux-aux-pair
                                       x (vector-ref x01 0) (vector-ref x01 1)
                                       y (vector-ref y01 0) (vector-ref y01 1)
                                       inner-equal-and-has-comment)}
                                    #f}]
               [else (assert-unreachable)]}})}
{define/t (value-force+equal?-aux-aux-pair x x0 x1 y y0 y1 inner-has-comment)
  (-> value-t value-t value-t value-t value-t value-t (box-t boolean-t) boolean-t)
  {let/t ([xy-equal-and-has-comment (box-t boolean-t) (box #f)])
         {if (and (value-force+equal?-aux x0 y0 xy-equal-and-has-comment) (value-force+equal?-aux x1 y1 xy-equal-and-has-comment))
             {begin
               {if (unbox xy-equal-and-has-comment)
                   (set-box! inner-has-comment #t)
                   (value-unsafe-set-to-just! x y)}
               #t}
             #f}}}

{define/t exp-id-s value-symbol-t (cons-value-symbol "標符")}

{define/t (evaluate space x)
  (-> identifierspace-t value-t value-t)
  ((evaluate-aux space x) id)}
{define/t (evaluate-aux space x)
  ;; m a = (cont-tt a value-t)
  (-> identifierspace-t value-t
      (cont-tt value-t value-t))
  {do cont->>=
    {<- x-comments (value-undelay-m x {λ () (vector space x)})}
    {:= x (vector-ref x-comments 0)}
    {:= comments (vector-ref x-comments 1)}
    {if (value-struct? x)
        {do cont->>=
          {:= ast (elim-value-struct x)}
          {<- ast-type--comments (value-undelay-m (vector-ref ast 0) {λ () (vector space x)})}
          {<- ast-list--comments (value-undelay-m (vector-ref ast 1) {λ () (vector space x)})}
          (WIP)
          }
        (cont-return (WIP))}}}
