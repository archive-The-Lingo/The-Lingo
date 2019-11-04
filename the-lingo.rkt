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

(define and-t and/c)
(define any-t any/c)
(define symbol-t symbol?)
(define vector-t vector/c)

(define nothing-t 'nothing)
(define/contract nothing nothing-t 'nothing)

(define t-id-t natural-number/c)
(define value-struct-t (vector-t t-id-t any-t any-t any-t))


(define value-symbol-t-id-t (and-t t-id-t 0))
(define/contract value-symbol-t-id value-symbol-t-id-t 0)
(define value-symbol-t (and-t value-struct-t (vector-t value-symbol-t-id-t symbol-t nothing-t nothing-t)))


(define/contract (create-value-symbol x)
  (-> symbol-t value-symbol-t)
  (vector value-symbol-t-id x nothing nothing))
