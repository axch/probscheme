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
 animal-tree-test
 (define-test (test-argument-strengths)
   ; Relies on zero branch lengths
   (assert-close 1 (specific-argument-strength '(gorilla) 'chimp))

   ; Empirical numbers, regression test
   (assert-close .5362731790150761
		 (specific-argument-strength '(gorilla) 'cow))
   (assert-close .892934664347785
		 (specific-argument-strength '(gorilla squirrel) 'cow))
   (assert-close .07133098723245616
		 (general-argument-strength '(gorilla squirrel)))))
