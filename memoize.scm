;;; ----------------------------------------------------------------------
;;; Copyright 2010 Alexey Radul
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

;;; Here's an unfinished experiment: How should I implement stochastic
;;; memoization?  It differs from normal memoization in important
;;; ways; as Vikash and Brian have discovered, it has semantics even
;;; on "pure" probabilistic functions.  And this implementation is not
;;; quite right.

(define (stochastic-memoize proc)
  (let ((memoized-alist '()))
    (lambda args
      (let ((binding (assoc args memoized-alist)))
	(if binding
	    ;; We've seen these arguments before, return the old value
	    (cdr binding)
	    (begin
	      ;; These arguments are fresh
	      (let ((new-binding (cons args #f)))
		(reversible-set! memoized-alist (cons new-binding memoized-alist))
		(let ((new-value (apply proc args)))
		  (set-cdr! new-binding new-value)
		  new-value))))))))
