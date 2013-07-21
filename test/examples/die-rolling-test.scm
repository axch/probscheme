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

(in-test-group
 die-rolling-test
 (define-test (test-two-dice)
   (let ((two-dice (distribution->alist
		    (stochastic-thunk->distribution
		     (lambda () (cons (roll-die) (roll-die)))))))
     (assert-equal 36 (length two-dice))
     (for-each (lambda (result)
		 (assert-equal 1/36 (cdr result)))
	       two-dice)))

 (define-test (test-sum)
   (assert-equal '((12 . 1/36) (11 . 1/18) (10 . 1/12) (9 . 1/9)
		   (8 . 5/36) (7 . 1/6) (6 . 5/36) (5 . 1/9)
		   (4 . 1/12) (3 . 1/18) (2 . 1/36))
		 (distribution->alist
		  (stochastic-thunk->distribution
		   (lambda () (+ (roll-die) (roll-die))))))
   (assert-equal 1/6 (probability-of
		      (stochastic-thunk->distribution
		       (lambda () (< (+ (roll-die) (roll-die)) 5)))
		      #t))
   (assert-equal 1/12 (probability-of
		       (stochastic-thunk->distribution
			(lambda () (= (+ (roll-die) (roll-die)) 4)))
		       #t)))

 (define-test (test-conditioning1)
   (assert-distribution
    '((10 . 1/2) (11 . 1/3) (12 . 1/6))
    (stochastic-thunk->distribution
     (lambda ()
       (let* ((x (roll-die))
	      (y (roll-die))
	      (sum (+ x y)))
	 (observe! (> sum 9))
	 sum)))))
 (define-test (test-conditioning2)
   (assert-distribution
    '((10 . 1/2) (11 . 1/3) (12 . 1/6))
    (conditional-distribution
     (independent-product die-roll-distribution
			  die-roll-distribution
			  (lambda (x y) (+ x y)))
     (lambda (sum) (> sum 9))))))

(define (roll-broken-die)
  (let ((face (roll-die)))
    (observe! (not (= face 1)))
    face))

(define (really-broken-die-dist)
  (conditional-distribution
   (stochastic-thunk->distribution roll-broken-die) 
   (lambda (face) (not (= face 6)))))

(define (nearly-useless-die)
  (let ((face (distribution-select (really-broken-die-dist))))
    (observe! (not (= face 3)))
    face))

(in-test-group
 die-rolling-test
 (define-test (test-broken-dice)
   (assert-distribution '((2 . 1/5) (3 . 1/5) (4 . 1/5) (5 . 1/5) (6 . 1/5))
			(stochastic-thunk->distribution roll-broken-die))
   (assert-distribution '((2 . 1/4) (3 . 1/4) (4 . 1/4) (5 . 1/4))
			(really-broken-die-dist))
   (assert-distribution '((2 . 1/3) (4 . 1/3) (5 . 1/3))
			(stochastic-thunk->distribution nearly-useless-die))))
