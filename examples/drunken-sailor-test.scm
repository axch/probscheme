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

;; Hm.  How the heck can I build an inference algorithm that would
;; discover the latent structure in this distribution, and avoid
;; searching an exponentially large tree of possible walk histories?

(define (unhindered-drunken-step place)
  (discrete-select ((- place 1) 1/2) ((+ place 1) 1/2)))

(define (hindered-drunken-step place)
  (if (zero? place)
      1 ; The blind drunk just bounces off the wall at 0.
      (discrete-select ((- place 1) 1/2) ((+ place 1) 1/2))))

(define (blind-drunken-wander step-function start num-steps)
  (if (zero? num-steps)
      start
      (blind-drunken-wander step-function
			    (step-function start)
			    (- num-steps 1))))

(define (blind-drunken-sailor-distribution start num-steps)
  (stochastic-thunk->distribution
   (lambda ()
     (blind-drunken-wander hindered-drunken-step start num-steps))))

(define (precient-drunken-wander step-function start num-steps)
  (observe! (>= start 0)) ; The precient drunk diligently avoids the wall at 0.
  (if (zero? num-steps)
      start
      (precient-drunken-wander step-function
			       (step-function start)
			       (- num-steps 1))))

(define (precient-drunken-sailor-distribution start num-steps)
  (stochastic-thunk->distribution
   (lambda ()
     (precient-drunken-wander unhindered-drunken-step start num-steps))))

(define (teleporting-drunken-sailor-distribution start num-steps)
  (conditional-distribution
   (stochastic-thunk->distribution
    (lambda ()
      (blind-drunken-wander unhindered-drunken-step start num-steps)))
   (lambda (end-result)
     ; The teleporting drunk magically never ends up negative
     (>= end-result 0))))

(define (check-drunken-wanderings
	 start num-steps blind-expected precient-expected teleporting-expected)
  (assert-distribution
   blind-expected
   (blind-drunken-sailor-distribution start num-steps))
  (assert-distribution
   precient-expected
   (precient-drunken-sailor-distribution start num-steps))
  (assert-distribution
   teleporting-expected
   (teleporting-drunken-sailor-distribution start num-steps)))

(in-test-group
 drunken-sailor
 (define-test (test-foresight-effect)
   (check-drunken-wanderings 0 1
    '((1 . 1))
    '((1 . 1))
    '((1 . 1)))
   (check-drunken-wanderings 1 2
    '((1 . 3/4) (3 . 1/4))
    '((1 . 2/3) (3 . 1/3))
    '((1 . 2/3) (3 . 1/3)))
   (check-drunken-wanderings 1 3
    '((0 . 3/8) (2 . 4/8) (4 . 1/8))
    '((0 . 2/6) (2 . 3/6) (4 . 1/6))
    '((0 . 3/7) (2 . 3/7) (4 . 1/7)))
   (check-drunken-wanderings 1 4
    '((1 . 10/16) (3 . 5/16) (5 . 1/16))
    '((1 . 5/10) (3 . 4/10) (5 . 1/10))
    '((1 . 6/11) (3 . 4/11) (5 . 1/11)))
   (check-drunken-wanderings 1 5
    '((0 . 10/32) (2 . 15/32) (4 . 6/32) (6 . 1/32))
    '((0 . 5/20) (2 . 9/20) (4 . 5/20) (6 . 1/20))
    '((0 . 10/26) (2 . 10/26) (4 . 5/26) (6 . 1/26)))
   )

 (define-test (test-selection1)
   (assert-distribution
    (distribution->alist (blind-drunken-sailor-distribution 0 10))
    (stochastic-thunk->distribution
     (lambda ()
       (distribution-select (blind-drunken-sailor-distribution 0 10))))))

 (define-test (test-selection2)
   (assert-distribution
    (distribution->alist (precient-drunken-sailor-distribution 0 10))
    (stochastic-thunk->distribution
     (lambda ()
       (distribution-select (precient-drunken-sailor-distribution 0 10))))))

 (define-test (test-selection3)
   (assert-distribution
    (distribution->alist (teleporting-drunken-sailor-distribution 0 10))
    (stochastic-thunk->distribution
     (lambda ()
       (distribution-select (teleporting-drunken-sailor-distribution 0 10)))))))
