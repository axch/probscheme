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
 more-animal-tree-impl-test
 (define-test (test-trivial)
   (define trivial-tree '(1 donkey))
   (let ((concept-dist ((concept-prior 0.1) trivial-tree)))
     (assert-close (probability-of concept-dist '())
		   (- 1 (turn-on-probability 0.1 1)))
     (assert-close (probability-of concept-dist '(donkey))
		   (turn-on-probability 0.1 1))))

 (define-test (test-simple)
   (define simple-tree '(1/2 (1/2 donkey) (1/2 camel)))
   (let ((concept-dist ((concept-prior 0.1) simple-tree))
	 (flip-prob (turn-on-probability 0.1 1/2)))
     (assert-close (probability-of concept-dist '())
		   (expt (- 1 flip-prob) 3))
     (assert-close (probability-of concept-dist '(donkey))
		   (* (expt (- 1 flip-prob) 2) flip-prob))
     (assert-close (probability-of concept-dist '(camel))
		   (* (expt (- 1 flip-prob) 2) flip-prob))
     (assert-close (probability-of concept-dist '(donkey camel))
		   (+ flip-prob
		      (* (- 1 flip-prob) flip-prob flip-prob))))))
