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

(define wasteful-coin
  (stochastic-thunk->distribution
   (lambda ()
     (let ((first (flip-coin))
	   (second (flip-coin)))
       (observe! (eq? first 'heads))
       second))))

(define uniform-answer
  '(((heads . heads) . 1/4)
    ((heads . tails) . 1/4)
    ((tails . heads) . 1/4)
    ((tails . tails) . 1/4)))

(in-test-group
 normalization-test

 (define-test (smoke)
   (assert-distribution
    uniform-answer
    (stochastic-thunk->distribution
     (lambda ()
       (cons (distribution-select wasteful-coin)
	     (flip-coin))))))

 (define-test (smoke2)
   (assert-distribution
    uniform-answer
   (stochastic-thunk->distribution
    (lambda ()
      (cons (distribution-select wasteful-coin)
	    (distribution-select wasteful-coin))))))

 (define-test (smoke3)
   (assert-distribution
    uniform-answer
    (stochastic-thunk->distribution
     (lambda ()
       (let ((first (flip-coin)))
	 (if (eq? 'heads first)
	     (cons first (flip-coin))
	     (cons first (distribution-select wasteful-coin))))))))


 (define-test (smoke4)
   (assert-distribution
    uniform-answer
    (stochastic-thunk->distribution
     (lambda ()
       (cons (distribution-splice wasteful-coin)
	     (flip-coin))))))

 (define-test (smoke5)
   (assert-distribution
    uniform-answer
    (stochastic-thunk->distribution
     (lambda ()
       (cons (distribution-splice wasteful-coin)
	     (distribution-splice wasteful-coin))))))

 (define-test (smoke6)
   (assert-distribution
    ;; !! This is because the splice causes the tails branch
    ;; to receive the negative evidence from the wasteful-coin
    '(((heads . heads) . 1/3)
      ((heads . tails) . 1/3)
      ((tails . heads) . 1/6)
      ((tails . tails) . 1/6))
    (stochastic-thunk->distribution
     (lambda ()
       (let ((first (flip-coin)))
	 (if (eq? 'heads first)
	     (cons first (flip-coin))
	     (cons first (distribution-splice wasteful-coin)))))))))
