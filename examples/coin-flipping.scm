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

;;; This is a little model of evaluating what one thinks of the
;;; fairness of a coin from watching the results of flipping it.  The
;;; coin is presumed to be fair (high prior probability), but could be
;;; rigged to show only one result (low prior probability); the
;;; posterior either determines that it cannot be rigged if it shows
;;; two different results, or leans increasingly towards assuming it
;;; is as flips keep showing the same result.

(define (prior)
  (discrete-select ('fair 98/100)
		   ('heads-only 1/100)
		   ('tails-only 1/100)))

(define (likelihood-model hypothesis datum)
  (likelihood! (case hypothesis
		 ((fair) 0.5)
		 ((heads-only) (if (eq? 'tails datum) 0 1.))
		 ((tails-only) (if (eq? 'heads datum) 0 1.)))))

(define (posterior data)
  (stochastic-thunk->distribution
   (lambda ()
     (let ((hypothesis (prior)))
       (for-each (lambda (datum)
		   (likelihood-model hypothesis datum))
		 data)
       hypothesis))))

#|
 (probability-of (posterior '(heads tails)) 'fair)

 ;Value: 1.

 (probability-of (posterior '(heads heads)) 'fair)

 ;Value: .9607843137254901

 (probability-of (posterior '(heads heads heads)) 'fair)

 ;Value: .924528301886792

 (probability-of (posterior '(heads heads heads heads)) 'fair)

 ;Value: .8596491228070171

 (probability-of (posterior '(heads heads heads heads heads)) 'fair)

 ;Value: .7538461538461534
|#
