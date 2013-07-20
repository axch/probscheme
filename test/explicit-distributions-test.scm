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

(define fair-coin (make-discrete-distribution '(heads 1/2) '(tails 1/2)))

(define two-coins (independent-product fair-coin fair-coin cons))

(in-test-group
 explicit-distributions-test
 (define-test (test-predicate)
   (for-each (lambda (thing)
	       (assert-false (distribution? thing)))
	     '(#t #f 3 67 "foo" () (bar baz) quux #(this is a vector)))
   (for-each (lambda (distribution)
	       (assert-true (distribution? distribution)))
	     (list fair-coin two-coins
		   (make-discrete-distribution '(foo 1))
		   (stochastic-thunk->distribution
		    (lambda () 'foo)))))

 (define-test (test-probability-of)
   (assert-eqv (probability-of fair-coin 'heads) 1/2)
   (assert-eqv (probability-of fair-coin 'foo) 0))

 (define-test (test-independent-product)
   (assert-eqv (probability-of two-coins '(heads . heads)) 1/4)
   (assert-eqv (probability-of two-coins '(heads . tails)) 1/4)
   (assert-eqv (probability-of two-coins '(tails . heads)) 1/4)
   (assert-eqv (probability-of two-coins '(tails . tails)) 1/4))

 (define-test (test-mixture)
   (let ((dist (mixture
		(list (make-discrete-distribution '(heads 1)) 1/2)
		(list (make-discrete-distribution '(tails 1)) 1/2))))
     (assert-eqv (probability-of dist 'heads) 1/2)
     (assert-eqv (probability-of dist 'tails) 1/2))

   (assert-equal (distribution->alist
		  (mixture
		   (list (make-discrete-distribution '(heads 1)) 1/2)
		   (list (make-discrete-distribution '(heads 1)) 1/2)))
		 '((heads . 1))))

 (define-test (test-truth-probability)
   (assert-eqv (truth-probability two-coins
				  (lambda (sequence)
				    (or (eq? 'heads (car sequence))
					(eq? 'heads (cdr sequence)))))
	       3/4))

 (define-test (test-mapping)
   (assert-distribution
    '(((heads . heads) . 1/2)
      ((tails . heads) . 1/4)
      ((tails . tails) . 1/4))
    (map-distribution
     two-coins
     (lambda (pair)
       (if (eq? 'heads (car pair))
	   (cons 'heads 'heads)
	   pair)))))

 (define-test (test-conditioning)
   (assert-distribution
    '(((heads . heads) . 1/3)
      ((heads . tails) . 1/3)
      ((tails . heads) . 1/3))
    (conditional-distribution
     two-coins
     (lambda (pair)
       (or (eq? 'heads (car pair))
	   (eq? 'heads (cdr pair)))))))
 
 (define-test (test-bayes-rule)
   (assert-distribution
    '(((heads . heads) . 9/16)
      ((heads . tails) . 3/16)
      ((tails . heads) . 3/16)
      ((tails . tails) . 1/16))
    (bayes-rule
     two-coins
     (lambda (pair)
       (let ((likelihood 1))
	 (if (eq? 'tails (car pair)) (set! likelihood 1/3))
	 (if (eq? 'tails (cdr pair)) (set! likelihood (* likelihood 1/3)))
	 likelihood)))))
 )

