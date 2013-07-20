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
 coin-flipping-test
 (define-test (test-simple-coin)
   (assert-distribution
    '((tails . 1))
    (stochastic-thunk->distribution
     (lambda ()
       (let ((face (discrete-select ('heads 1/2) ('tails 1/2))))
	 (observe! (eq? face 'tails))
	 face)))))

 (define-test (test-coin-flipping)
   (assert-close 1 (probability-of (posterior '(tails heads)) 'fair))
   (assert-= 0 (probability-of (posterior '(tails heads)) 'heads-only))
   (assert-= 0 (probability-of (posterior '(tails heads)) 'tails-only))

   (assert-close 0.98 (probability-of (posterior '(tails)) 'fair))
   (assert-close 0.02 (probability-of (posterior '(tails)) 'tails-only))
   (assert-= 0 (probability-of (posterior '(tails)) 'heads-only))

   (assert-close 0.98 (probability-of (posterior '(heads)) 'fair))
   (assert-close 0.02 (probability-of (posterior '(heads)) 'heads-only))
   (assert-= 0 (probability-of (posterior '(heads)) 'tails-only))

   (assert-close 0.2768361581920904 
		 (probability-of (posterior '(tails tails tails tails
						    tails tails tails tails))
				 'fair))))
