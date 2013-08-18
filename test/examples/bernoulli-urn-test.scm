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
 bernoulli-urn-test
 (define-test (bernoulli-urns)
   (assert-equal 1/8 (probability-of-sequence
		      '(red red red) joyce-draw (make-urn 3 3)))
   (assert-equal 1/8 (probability-of-sequence
		      '(red red blue) joyce-draw (make-urn 3 3)))
   (assert-equal 1/10 (probability-of-sequence
		       '(red blue blue) joyce-draw (make-urn 3 3)))
   (assert-equal 1/20 (probability-of-sequence
		       '(blue blue blue) joyce-draw (make-urn 3 3)))))
