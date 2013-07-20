;;; ----------------------------------------------------------------------
;;; Copyright 2007 Alexey Radul, Taylor Campbell, and Yu-hsin Chen.
;;; ----------------------------------------------------------------------
;;; This file is part of Probabilistic Scheme.
;;; 
;;; Probabilistic Scheme is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;; 
;;; Probabilistic Scheme is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with Probabilistic Scheme.  If not, see <http://www.gnu.org/licenses/>.
;;; ----------------------------------------------------------------------

(declare (usual-integrations))

;;;; LIFO Stacks

(define-structure (stack (constructor make-stack ()))
  (pointer '()))

(define (stack-empty? stack)
  (null? (stack-pointer stack)))

(define (stack-top stack)
  (car (stack-pointer stack)))

(define (stack-push! stack element)
  (set-stack-pointer! stack (cons element (stack-pointer stack))))

(define (stack-pop! stack)
  (set-stack-pointer! stack (cdr (stack-pointer stack))))
