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
(require racket/contract)
(define-syntax-rule (define:type . xs) (define . xs))
(define-syntax-rule (define/t . xs) (define/contract . xs))

(define:type and-t and/c)
(define:type or-t or/c)
(define:type any-t any/c)
(define:type symbol-t symbol?)
(define:type char-t char?)
(define:type vector-t vector/c)
(define:type void-t void?)

(define:type nothing-t 'nothing)
(define/t nothing nothing-t 'nothing)

(define:type t-id-t natural-number/c)
(define:type value-struct-t (vector-t t-id-t any-t any-t any-t))
(define:type value-t
  (or-t
   (recursive-contract value-symbol-t #:impersonator)))

(define:type value-symbol-t-id-t (and-t t-id-t 0))
(define/t value-symbol-t-id value-symbol-t-id-t 0)
(define:type value-pair-t-id-t (and-t t-id-t 1))
(define/t value-pair-t-id value-pair-t-id-t 1)
(define:type value-null-t-id-t (and-t t-id-t 2))
(define/t value-null-t-id value-null-t-id-t 2)
(define:type value-data-t-id-t (and-t t-id-t 3))
(define/t value-data-t-id value-data-t-id-t 3)
(define:type value-char-t-id-t (and-t t-id-t 4))
(define/t value-char-t-id value-char-t-id-t 4)
(define:type value-just-t-id-t (and-t t-id-t 5))
(define/t value-just-t-id value-just-t-id-t 5)

(define:type value-symbol-t (and-t value-struct-t (vector-t value-symbol-t-id-t symbol-t nothing-t nothing-t)))
(define:type value-pair-t (and-t value-struct-t (vector-t value-pair-t-id-t value-t value-t nothing-t)))
(define:type value-null-t (and-t value-struct-t (vector-t value-null-t-id-t nothing-t nothing-t nothing-t)))
(define:type value-data-t (and-t value-struct-t (vector-t value-data-t-id-t value-t value-t nothing-t)))
(define:type value-char-t (and-t value-struct-t (vector-t value-char-t-id-t char-t nothing-t nothing-t)))
(define:type value-just-t (and-t value-struct-t (vector-t value-just-t-id-t value-t nothing-t nothing-t)))

(define/t (cons-value-symbol x)
  (-> symbol-t value-symbol-t)
  (vector value-symbol-t-id x nothing nothing))
(define/t (elim-value-symbol x)
  (-> value-symbol-t symbol-t)
  (vector-ref x 1))
(define/t (cons-value-pair x y)
  (-> value-t value-t value-pair-t)
  (vector value-pair-t-id x y nothing))
(define/t (elim-value-pair x)
  (-> value-pair-t (vector-t value-t value-t))
  (vector (vector-ref x 1) (vector-ref x 2)))
(define/t (cons-value-data x y)
  (-> value-t value-t value-data-t)
  (vector value-data-t-id x y nothing))
(define/t (elim-value-data x)
  (-> value-data-t (vector-t value-t value-t))
  (vector (vector-ref x 1) (vector-ref x 2)))
(define/t value-null value-null-t (vector value-null-t-id nothing nothing nothing))
(define/t (cons-value-char x)
  (-> char-t value-char-t)
  (vector value-char-t-id x nothing nothing))
(define/t (elim-value-char x)
  (-> value-char-t char-t)
  (vector-ref x 1))

(define/t (value-unsafe-set-to-just! x v)
  (-> value-t value-t void-t)
  (vector-set! x 0 value-just-t-id)
  (vector-set! x 1 v)
  (vector-set! x 2 nothing)
  (vector-set! x 3 nothing))
