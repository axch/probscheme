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
 math-problems
 (define-test (cards-and-sides)
   ;; Suppose I have three cards: One is red on both sides, one is
   ;; blue on both sides, and one is red on one side and blue on the
   ;; other.  I shuffle the cards and put one on the table.  The side
   ;; facing up turns out to be red.  What is the probability that the
   ;; other side is red, too?
   (let ((card-list '((red . red) (blue . blue) (red . blue))))
     (let ((other-side-dist
	    (stochastic-thunk->distribution
	     (lambda ()
	       (let* ((card (uniform-select card-list))
		      (side-shown ((uniform-select (list car cdr)) card))
		      (other-side (if (eq? side-shown (car card))
				      (cdr card)
				      (car card))))
		 (observe! (eq? side-shown 'red))
		 other-side)))))
       (assert-distribution
	'((red . 2/3) (blue . 1/3))
	other-side-dist)))))
