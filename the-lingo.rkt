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
(define:type vector-t vector/c)

(define:type nothing-t 'nothing)
(define/t nothing nothing-t 'nothing)

(define:type t-id-t natural-number/c)
(define:type value-struct-t (vector-t t-id-t any-t any-t any-t))
(define:type value-t
  (or-t
   (recursive-contract value-symbol-t #:impersonator)))

(define:type value-symbol-t-id-t (and-t t-id-t 0))
(define/t value-symbol-t-id value-symbol-t-id-t 0)
(define:type value-symbol-t (and-t value-struct-t (vector-t value-symbol-t-id-t symbol-t nothing-t nothing-t)))

(define/t (cons-value-symbol x)
  (-> symbol-t value-symbol-t)
  (vector value-symbol-t-id x nothing nothing))
(define/t (elim-value-symbol x)
  (-> value-symbol-t symbol-t)
  (vector-ref x 1))
