;;; ----------------------------------------------------------------------
;;; Copyright 2007-2010 Alexey Radul, Taylor Campbell, and Yu-hsin Chen.
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

;;;; Standard Distributions

(define (bernoulli-distribution #!optional probability)
  (if (default-object? probability) (set! probability 1/2))
  (make-discrete-distribution `(#t ,probability) `(#f ,(- 1 probability))))

(define (uniform-distribution objects)
  (alist->distribution
   (let ((mass (/ 1 (length objects))))
     (map (lambda (thing) (cons thing mass))
	  objects))))

(define (singleton-distribution thing)
  (uniform-distribution (list thing)))

(define (flip yes no prob)
  (cond ((= prob 1)
	 (singleton-distribution yes))
	((= prob 0)
	 (singleton-distribution no))
	(else
	 (make-discrete-distribution `(,yes ,prob) `(,no ,(- 1 prob))))))

(define die-roll-distribution
  (uniform-distribution '(1 2 3 4 5 6)))

(define (bernoulli-trials n #!optional prob)
  (if (default-object? prob)
      (set! prob 1/2))
  (define (the-binomial-coefficients n)
    (let loop ((result '()) (k n) (coeff 1))
      (if (< k 0)
	  result
	  (loop (cons coeff result)
		(- k 1)
		(* coeff k (/ 1 (- n (- k 1))))))))
  (cond ((= prob 1) (singleton-distribution n))
	((= prob 0) (singleton-distribution 0))
	(else
	 (alist->distribution
	  (map (lambda (k coeff)
		 (cons k (* coeff (expt prob k) (expt (- 1 prob) (- n k)))))
	       (iota (+ n 1))
	       (the-binomial-coefficients n))))))

;;;; Standard Implicit Helpers

(define (boolean-select probability)
  (discrete-select (#t probability) (#f (- 1 probability))))

(define (uniform-select objects)
  (distribution-select (uniform-distribution objects)))

(define (flip-coin)
  (discrete-select ('heads 1/2) ('tails 1/2)))

(define (roll-die #!optional number-of-sides)
  (if (default-object? number-of-sides) (set! number-of-sides 6))
  (define (integers-between low high)
    (if (> low high)
	'()
	(cons low (integers-between (+ low 1) high))))
  (uniform-select (integers-between 1 number-of-sides)))

(define (bernoulli-select n #!optional prob)
  (distribution-select (bernoulli-trials n prob)))

;;;; Forcing accessors

(define (truth-probability distribution predicate)
  (probability-of
   (map-distribution
    distribution 
    (lambda (datum)
      (and (predicate datum) #t))) ; Force #t or #f
   #t))

(define (probability-of distribution datum)
  (distribution/determine! distribution)
  (distribution/datum-mass distribution datum))

(define (determined! distribution)
  (distribution/determine! distribution)
  distribution)

(define (normalize-determine! distribution)
  (distribution/determine! distribution)
  (let ((result (distribution/normalize distribution)))
    (distribution/determine! result)
    result))

(define (determined-select-from-thunk thunk)
  (distribution-select (determined! (stochastic-thunk->distribution thunk))))

;; Also define distribution->current-data-list,
;; distribution->current-mass-alist?
(define (distribution->alist distribution)
  (distribution->current-density-alist (normalize-determine! distribution)))

(define (distribution->sorted-alist distribution)
  (sort (distribution->alist distribution)
	(lambda (pair1 pair2)
	  (> (cdr pair1) (cdr pair2)))))

(define (distribution->interesting-alist distribution #!optional accuracy)
  (if (default-object? accuracy)
      (set! accuracy .01))
  (let loop ((result '())
	     (remainder (distribution->sorted-alist distribution))
	     (mass-left 1))
    (if (> mass-left accuracy)
	(loop (cons (car remainder) result)
	      (cdr remainder)
	      (- mass-left (cdar remainder)))
	(reverse (cons (cons #!default (apply + (map cdr remainder)))
		       result)))))

;;;; Fully determined combinators

(define (dist-reduce combiner id distributions)
  (reduce (lambda (running-dist new-dist)
	    (determined! (independent-product running-dist new-dist combiner)))
	  (singleton-distribution id)
	  distributions))

(define (dist-sum distributions)
  (dist-reduce + 0 distributions))

(define (dist-expt distribution n combiner)
  (cond ((= n 1)
	 distribution)
	((even? n)
	 (let ((answer (determined! (independent-product
				     distribution distribution combiner))))
	   (determined! (dist-expt answer (/ n 2) combiner))))
	((odd? n)
	 (determined!
	  (independent-product
	   distribution (dist-expt distribution (- n 1) combiner) combiner)))
	(else
	 (error "dist-expt weird power" n))))

;;;; More determined stuff

(define (expectation distribution #!optional function)
  (if (not (default-object? function))
      (expectation (map-distribution distribution function))
      ;; TODO not fully determined distributions?
      (reduce + 0
       (map (lambda (pair)
	      (* (car pair) (cdr pair)))
	    (distribution->alist distribution)))))

(define (variance distribution #!optional function)
  (if (not (default-object? function))
      (variance (map-distribution distribution function))
      (- (expectation distribution square)
	 (square (expectation distribution)))))

(define (standard-deviation distribution #!optional function)
  (sqrt (variance distribution function)))
