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
 implicit-distributions-test

 (define-test (coin-flip)
   (let ((two-coin-dist
	  (stochastic-thunk->distribution
	   (lambda ()
	     (list (flip-coin) (flip-coin))))))
     (distribution/determine! two-coin-dist)
     (assert-true 
      (lset= equal? '(
		      ((heads heads) . 1/4)
		      ((heads tails) . 1/4)
		      ((tails heads) . 1/4)
		      ((tails tails) . 1/4)
		      )
	     (distribution->current-density-alist two-coin-dist)))
     (assert-equal
      1
      (distribution/min-normalizer two-coin-dist))
     (assert-equal
      1
      (distribution/max-normalizer two-coin-dist))
     ))

 ;; test mass
 (define-test (die-roll)
   (let ((two-die-dist
	  (stochastic-thunk->distribution
	   (lambda ()
	     (cons (roll-die) (roll-die))))))
     (let loop ((n 5))
       (if (> n 0)
	   (begin
	     (distribution/refine! two-die-dist)
	     (loop (- n 1)))))
     (assert-equal
      5/36
      (distribution/min-normalizer two-die-dist))
     (let loop ((n 5))
       (if (> n 0)
	   (begin
	     (distribution/refine! two-die-dist)
	     (loop (- n 1)))))
     (assert-equal
      5/18
      (distribution/min-normalizer two-die-dist))
     ))

 ;;; bernoulli urns

 ;; start with an urn with 3 red and 3 blue balls (make-urn 3 3)
 ;; remove three balls randomly from the urn without replacement
 (define-test (bernoulli-urn1)
   (let ((urn-dist
	  (stochastic-thunk->distribution
	   (lambda ()
	     (draw-sequence draw-random-ball (make-urn 3 3) 3)))))
     (distribution/determine! urn-dist)
     (assert-fully-forced urn-dist)
     (assert-equal
      1/20
      (distribution/datum-density urn-dist '(red red red)))
     (assert-equal
      1/20
      (distribution/datum-density urn-dist '(blue blue blue)))
     ))

 ;; blue balls are drawn without replacement, while red balls are drawn
 ;; with replacement.
 (define-test (bernoulli-urn2)
   (let ((urn-dist
	  (stochastic-thunk->distribution
	   (lambda ()
	     (draw-sequence joyce-draw (make-urn 3 3) 3)))))
     (distribution/determine! urn-dist)
     (assert-fully-forced urn-dist)
     (assert-equal
      1/8
      (distribution/datum-density urn-dist '(red red red)))
     (assert-equal
      1/20
      (distribution/datum-density urn-dist '(blue blue blue)))
     (assert-equal
      1/10
      (distribution/datum-density urn-dist '(red blue blue)))
     ))

 ;; Something with observe!
 (define-test (observations)
   (let ((broken-die-dist
	  (stochastic-thunk->distribution
	   (lambda ()
	     (let ((number (roll-die)))
	       (observe! (even? number))
	       number)))))
     (distribution/determine! broken-die-dist)
     (assert-fully-forced broken-die-dist)
     (for-each
      (lambda (num)
	(assert-equal
	 (if (even? num) 1/6 0)
	 (distribution/datum-density broken-die-dist num)))
      '(1 2 3 4 5 6))
     (assert-equal 1/2 (distribution/min-normalizer broken-die-dist))
     (assert-equal 1/2 (distribution/max-normalizer broken-die-dist))))

 (define-test (observations-2)
   (let ((broken-die-dist
	  (stochastic-thunk->distribution
	   (lambda ()
	     (let ((number (roll-die)))
	       (observe! (even? number))
	       number))
           make-breadth-first-schedule)))
     (distribution/determine! broken-die-dist)
     (assert-fully-forced broken-die-dist)
     (for-each
      (lambda (num)
	(assert-equal
	 (if (even? num) 1/6 0)
	 (distribution/datum-density broken-die-dist num)))
      '(1 2 3 4 5 6))
     (assert-equal 1/2 (distribution/min-normalizer broken-die-dist))
     (assert-equal 1/2 (distribution/max-normalizer broken-die-dist))))

 (define-test (observations-3)
   (let ((broken-die-dist
          (stochastic-thunk->distribution
           (lambda ()
             (let ((number (roll-die)))
               (observe! (even? number))
               number))
           make-priority-schedule)))
     (distribution/determine! broken-die-dist)
     (assert-fully-forced broken-die-dist)
     (for-each
      (lambda (num)
        (assert-equal
         (if (even? num) 1/6 0)
         (distribution/datum-density broken-die-dist num)))
      '(1 2 3 4 5 6))
     (assert-equal 1/2 (distribution/min-normalizer broken-die-dist))
     (assert-equal 1/2 (distribution/max-normalizer broken-die-dist))))

 (define-test (impossibilities)
   (let ((impossible-finite-dist
          (stochastic-thunk->distribution
           (lambda ()
             (let ((number (roll-die)))
               (observe! (> number 6))
               number)))))
     (distribution/refine-to-density-bound! impossible-finite-dist 1/2)
     ;; Impossible distributions are harmless (if uninformative) as
     ;; long as the machine doesn't know they're impossible:
     (check (equal? 0 (distribution/datum-min-mass impossible-finite-dist 2)))
     (check (equal? 1 (distribution/datum-max-mass impossible-finite-dist 2)))
     (check (<= (distribution/undetermined-density impossible-finite-dist) 1/2))))

 (define-test (distribution-select-smoke)
   (let* ((die-roll-dist (stochastic-thunk->distribution roll-die))
	  (die-roll-dist2
	   (stochastic-thunk->distribution
	    (lambda ()
	      (distribution-select die-roll-dist)))))
     (assert-distribution (distribution->alist die-roll-dist)
			  die-roll-dist2)
     (assert-fully-forced die-roll-dist)
     (assert-fully-forced die-roll-dist2)))

 (define-test (implicit-language-nesting)
   (let* ((nested-distribution
	   (stochastic-thunk->distribution
	    (lambda ()
	      (stochastic-thunk->distribution
	       (lambda () 'foo)))))
	  (nested-alist (distribution->alist nested-distribution)))
     (assert-true (distribution? nested-distribution))
     (assert-= 1 (length nested-alist))
     (assert-= 1 (cdar nested-alist))
     (assert-true (distribution? (caar nested-alist)))
     (assert-distribution '((foo . 1)) (caar nested-alist))))

 (define-test (non-collapse-test)
   (let ((meta-distribution
	  (stochastic-thunk->distribution
	   (lambda ()
	     (let ((fair (make-discrete-distribution '(heads 1/2) '(tails 1/2)))
		   (heads-only (make-discrete-distribution '(heads 1))))
	       (discrete-select (fair 1/3) (heads-only 2/3)))))))
     (assert-true (distribution? meta-distribution))
     (let ((possibilities (distribution->alist meta-distribution)))
       (assert-= 2 (length possibilities))
       (assert-true (distribution? (caar possibilities)))
       (assert-true (distribution? (caadr possibilities))))
     (let ((flattening
	    (stochastic-thunk->distribution
	     (lambda ()
	       (distribution-select (distribution-select meta-distribution))))))
       (assert-distribution
	'((heads . 5/6)
	  (tails . 1/6))
	flattening))))

 (define-test (more-nesting)
   (let* ((nested-distribution
	   (stochastic-thunk->distribution
	    (lambda ()
	      (discrete-select
	       ((stochastic-thunk->distribution
		 (lambda () (discrete-select ('heads 1/2) ('tails 1/2)))) 1/5)
	       ((stochastic-thunk->distribution
		 (lambda () (discrete-select (1 1/3) (2 1/3) (3 1/3)))) 4/5)))))
	  (nested-alist (distribution->alist nested-distribution)))
     (assert-true (distribution? nested-distribution))
     (assert-= 2 (length nested-alist))
     (assert-true (or (= 1/5 (cdar nested-alist))
		      (= 4/5 (cdar nested-alist))))
     (assert-true (or (= 1/5 (cdadr nested-alist))
		      (= 4/5 (cdadr nested-alist))))
     (for-each (lambda (object)
		 (assert-true (distribution? object)))
	       (map car nested-alist))
     (assert-distribution
      '((heads . 1/10)
	(tails . 1/10)
	(1 . 4/15)
	(2 . 4/15)
	(3 . 4/15))
      (stochastic-thunk->distribution
       (lambda ()
	 (distribution-select (distribution-select nested-distribution)))))))

 (define-test (nesting-and-partial-refinement)
   (let ((nested-distribution
	  (stochastic-thunk->distribution
	   (lambda ()
	     (let ((variant (discrete-select ('a 1/5) ('b 4/5))))
	       (case variant
		 ((a) (let ((coin (stochastic-thunk->distribution
				   (lambda ()
				     (discrete-select ('heads 1/2) ('tails 1/2))))))
			(distribution/refine! coin)
			coin))
		 ((b) (let ((number (stochastic-thunk->distribution
				     (lambda ()
				       (discrete-select (1 1/3) (2 1/3) (3 1/3))))))
			(distribution/refine! number)
			(distribution/refine! number)
			number))))))))
     (distribution/refine! nested-distribution)
     (let ((internal-dist (possibility/datum
			   (stream-car (distribution->mass-stream nested-distribution)))))
       (assert-true (distribution? internal-dist))
       (assert-false (distribution/determined? internal-dist))
       (distribution/refine! internal-dist)
       (assert-true (distribution/determined? internal-dist)))
     (assert-distribution
      '((heads . 1/10)
	(tails . 1/10)
	(1 . 4/15)
	(2 . 4/15)
	(3 . 4/15))
      (stochastic-thunk->distribution
       (lambda ()
	 (distribution-select (distribution-select nested-distribution)))))))
 )

