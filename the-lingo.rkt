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
{require racket/contract}
{define-syntax-rule {define:type . xs} {define . xs}}
{define-syntax-rule {define/t . xs} {define/contract . xs}}
{define-syntax let/t
  {syntax-rules ()
    [(_ () . b) ({λ () . b})]
    [(_ ([id typ val] . rs) . b) ({λ () {define/t id typ val} {let/t rs . b}})]}}
{define-syntax-rule (rec-type x) (recursive-contract x #:chaperone)}
{define:type and-t and/c}
{define:type or-t or/c}
{define:type any-t any/c}
{define:type symbol-t symbol?}
{define:type char-t char?}
{define:type vector-t vector/c}
{define:type void-t void?}
{define:type boolean-t boolean?}
{define:type hash-t hash/c}
{define:type arrayof-t listof} ;; A array is a variable length vector
{define (create-array . xs) xs}
{define (linear-array-add-element xs x) (cons x xs)} ;; Add a element to a array.Can destory source array."linear-" means linear type system
{define (linear-array-append xs ys) (append xs ys)}
{define-syntax-rule {array-foreach xs v . c} {for ([v xs]) . c}}

{define:type nothing-t void-t}
{define/t nothing nothing-t (void)}

{define:type t-id-t natural-number/c}
{define:type value-struct-t (vector-t t-id-t any-t any-t any-t)}
{define:type value-t
  (or-t
   (rec-type value-symbol-t)
   (rec-type value-pair-t)
   (rec-type value-null-t)
   (rec-type value-data-t)
   (rec-type value-char-t)
   (rec-type value-just-t)
   (rec-type value-delay-t)
   (rec-type value-comment-t)
   )}
{define:type env-t (hash-t value-t value-t)}

{define:type value-symbol-t-id-t (and-t t-id-t 0)}
{define/t value-symbol-t-id value-symbol-t-id-t 0}
{define:type value-pair-t-id-t (and-t t-id-t 1)}
{define/t value-pair-t-id value-pair-t-id-t 1}
{define:type value-null-t-id-t (and-t t-id-t 2)}
{define/t value-null-t-id value-null-t-id-t 2}
{define:type value-data-t-id-t (and-t t-id-t 3)}
{define/t value-data-t-id value-data-t-id-t 3}
{define:type value-char-t-id-t (and-t t-id-t 4)}
{define/t value-char-t-id value-char-t-id-t 4}
{define:type value-just-t-id-t (and-t t-id-t 5)}
{define/t value-just-t-id value-just-t-id-t 5}
{define:type value-delay-t-id-t (and-t t-id-t 6)}
{define/t value-delay-t-id value-delay-t-id-t 6}
{define:type value-comment-t-id-t (and-t t-id-t 7)}
{define/t value-comment-t-id value-comment-t-id-t 7}

{define:type value-symbol-t (and-t value-struct-t (vector-t value-symbol-t-id-t symbol-t nothing-t nothing-t))}
{define:type value-pair-t (and-t value-struct-t (vector-t value-pair-t-id-t value-t value-t nothing-t))}
{define:type value-null-t (and-t value-struct-t (vector-t value-null-t-id-t nothing-t nothing-t nothing-t))}
{define:type value-data-t (and-t value-struct-t (vector-t value-data-t-id-t value-t value-t nothing-t))}
{define:type value-char-t (and-t value-struct-t (vector-t value-char-t-id-t char-t nothing-t nothing-t))}
{define:type value-just-t (and-t value-struct-t (vector-t value-just-t-id-t value-t nothing-t nothing-t))}
{define:type value-delay-t (and-t value-struct-t (vector-t value-delay-t-id-t (-> value-t) (-> (vector-t (rec-type env-t) value-t)) nothing-t))} ;; exec:`(-> value-t)`/display:`(-> (vector-t env-t value-t))`
{define:type value-comment-t (and-t value-struct-t (vector-t value-comment-t-id-t value-t (arrayof-t value-t) nothing-t))} ;; val:value-t/comment:`(arrayof-t value-t)`

{define/t (value-symbol? x)
  (-> value-t boolean-t)
  (= (vector-ref x 0) value-symbol-t-id)}
{define/t (value-pair? x)
  (-> value-t boolean-t)
  (= (vector-ref x 0) value-pair-t-id)}
{define/t (value-null? x)
  (-> value-t boolean-t)
  (= (vector-ref x 0) value-null-t-id)}
{define/t (value-data? x)
  (-> value-t boolean-t)
  (= (vector-ref x 0) value-data-t-id)}
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
  (-> symbol-t value-symbol-t)
  (vector value-symbol-t-id x nothing nothing)}
{define/t (elim-value-symbol x)
  (-> value-symbol-t symbol-t)
  (vector-ref x 1)}
{define/t (cons-value-pair x y)
  (-> value-t value-t value-pair-t)
  (vector value-pair-t-id x y nothing)}
{define/t (elim-value-pair x)
  (-> value-pair-t (vector-t value-t value-t))
  (vector (vector-ref x 1) (vector-ref x 2))}
{define/t (cons-value-data x y)
  (-> value-t value-t value-data-t)
  (vector value-data-t-id x y nothing)}
{define/t (elim-value-data x)
  (-> value-data-t (vector-t value-t value-t))
  (vector (vector-ref x 1) (vector-ref x 2))}
{define/t value-null value-null-t (vector value-null-t-id nothing nothing nothing)}
{define/t (cons-value-char x)
  (-> char-t value-char-t)
  (vector value-char-t-id x nothing nothing)}
{define/t (elim-value-char x)
  (-> value-char-t char-t)
  (vector-ref x 1)}
{define/t (cons-value-comment x comment)
  (-> value-t (arrayof-t value-t) value-comment-t)
  (vector value-comment-t-id x comment nothing)}
{define/t (elim-value-comment x)
  (-> value-comment-t (vector-t value-t (arrayof-t value-t)))
  (vector (vector-ref x 1) (vector-ref x 2))}

{define/t (value-unsafe-set-to-just! x v)
  (-> value-t value-t void-t)
  (if (eq? x v) (void)
      (vector-set*! x
                    0 value-just-t-id
                    1 v
                    2 nothing
                    3 nothing))}
{define/t (must-value-unjust-1 x)
  (-> value-just-t value-t)
  (vector-ref x 1)}
{define/t (value-unjust-* x)
  (-> value-t value-t)
  (value-unjust-*-aux x (create-array x))}
{define/t (value-unjust-*-aux x history)
  (-> value-t (arrayof-t value-t) value-t)
  (if (value-just? x) (value-unjust-*-aux (must-value-unjust-1 x) (linear-array-add-element history x))
      {begin
        {array-foreach history history_v (value-unsafe-set-to-just! history_v x)}
        x})}
{define/t (cons-value-delay exec display_f)
  (-> (-> value-t) (-> (vector-t env-t value-t)) value-delay-t)
  (vector value-delay-t-id exec display_f nothing)}
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
  (value-force-aux x (create-array x))}
{define/t (value-force-aux x history)
  (-> value-t (arrayof-t value-t) value-t)
  {cond
    [(value-just? x) (value-force-aux (must-value-unjust-1 x) (linear-array-add-element history x))]
    [(value-delay? x) (value-force-aux (must-nocache-value-force-1 x) (linear-array-add-element history x))]
    [else
     {array-foreach history history_v (value-unsafe-set-to-just! history_v x)}
     x]}}
{define/t (value-undelay-1_>>= x display_f f)
  (-> value-t (-> (vector-t env-t value-t)) (-> value-t (arrayof-t value-t) value-t) value-t)
  (value-undelay-1_>>=_aux x display_f (create-array) f)}
{define/t (value-undelay-1_>>=_aux x display_f comments f)
  (-> value-t (-> (vector-t env-t value-t)) (arrayof-t value-t) (-> value-t (arrayof-t value-t) value-t) value-t)
  {cond
    [(value-comment? x)
     {let/t ([r (vector-t value-t (arrayof-t value-t)) (elim-value-comment x)])
            {let/t ([new-v value-t (vector-ref r 0)] [new-comments (arrayof-t value-t) (vector-ref r 1)])
                   (value-undelay-1_>>=_aux new-v (linear-array-append comments new-comments) f)}}]
    [(value-just? x) (value-undelay-1_>>=_aux (value-unjust-* x) display_f comments f)]
    [(value-delay? x) (cons-value-delay {λ () (value-undelay-1_>>=_aux (must-value-force-1 x) display_f comments f)} display_f)]
    [else (f x)]}}
