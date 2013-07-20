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

(define (geometric-select alpha start)
  (discrete-select
   (start alpha)
   ((geometric-select alpha (+ start 1)) (- 1 alpha))))

(define (simple-conditioned-dist #!optional predicate)
  (if (default-object? predicate) (set! predicate (lambda (x) #t)))
  (stochastic-thunk->distribution
   (lambda ()
     (let ((num (geometric-select 1/2 0)))
       (observe! (predicate num))
       num))))

(in-test-group
 geometric-test

 (define-test (test-smoke)
   (for-each (assert-prob-converges (simple-conditioned-dist) 1e-10)
	     '(0   1   2   3    4)
	     '(1/2 1/4 1/8 1/16 1/32)))

 (define-test (test-simple-condition)
   (for-each (assert-prob-converges
	      (simple-conditioned-dist (lambda (num) (> num 0)))
	      1e-10)
	     '(0   1   2   3   4)
	     '(0   1/2 1/4 1/8 1/16)))

 (define-test (test-odd-condition)
   (for-each (assert-prob-converges (simple-conditioned-dist odd?) 1e-10)
	     '(0   1   2   3    4   5)
	     (map (lambda (density)
		    (/ density 1/3))
		  '(0   1/4 0   1/16 0   1/64))))
 
 (define-test (test-odd-quarter)
   (let ((reparametrized-distribution
	  (stochastic-thunk->distribution
	   (lambda ()
	     (let ((num (geometric-select 1/4 0)))
	       (observe! (odd? num))
	       num)))))
     (for-each (assert-prob-converges reparametrized-distribution 1e-10)
	       '(0   1   2   3    4   5)
	       (map (lambda (density)
		      (/ density 3/7))
		    '(0   3/16 0   27/256 0   243/4096)))))
 
 (define-test (test-deep-computation)
   (for-each (assert-prob-converges
	      (simple-conditioned-dist (lambda (num) (> num 500))) 1e-10)
	     '(500 501 502 503 504)
	     '(0   1/2 1/4 1/8 1/16)))

 (define-test (test-deep-recovery)
   (let ((dist (simple-conditioned-dist)))
     (for-each (assert-prob-converges dist 1e-10)
	       '(0   1   2   3    4)
	       '(1/2 1/4 1/8 1/16 1/32))
     (let ((dist2 (conditional-distribution dist (lambda (num) (> num 500)))))
       (for-each (assert-prob-converges dist2 1e-10)
		 '(500 501 502 503 504)
		 '(0   1/2 1/4 1/8 1/16))))))

