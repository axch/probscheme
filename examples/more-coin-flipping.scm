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

(define (prior)
  (make-discrete-distribution '(fair 98/100)
			      '(heads-only 1/100)
			      '(tails-only 1/100)))

(define (likelihood-model hypothesis datum)
  (case hypothesis
    ((fair) 0.5)
    ((heads-only) (if (eq? 'tails datum) 0 1.))
    ((tails-only) (if (eq? 'heads datum) 0 1.))))

(define (posterior data)
  (bayes-rule (prior) (lambda (hypothesis)
			(reduce *
				1
				(map (lambda (datum)
				       (likelihood-model hypothesis datum))
				     data)))))
