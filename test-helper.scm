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

(declare (usual-integrations))

;;;; Assertions

(define (assert-close num1 num2)
  (assert-in-delta num1 num2 1e-10))

(define (general-< foo bar)
  (let ((type-alist `((,symbol? . ,symbol<?)
		      (,number? . ,<)
		      (,null? . ,(lambda (a b)
				   #t)))))
    (let loop ((types type-alist))
      (if (null? types) 
	  (error "Ran out of types" foo bar)
	  (let ((predicate (caar types))
		(comparator (cdar types)))
	    (cond ((and (predicate foo) (predicate bar))
		   (comparator foo bar))
		  ((predicate foo) #t)
		  ((predicate bar) #f)
		  ((and (pair? foo) (pair? bar))
		   (cond ((general-< (car foo) (car bar))
			  #t)
			 ((general-< (car bar) (car foo))
			  #f)
			 (else (general-< (cdr foo)
					  (cdr bar)))))
		  (else (loop (cdr types)))))))))

(define (sort-alist alist)
  (define (alist-entry-< foo bar)
    (general-< (car foo) (car bar)))
  (sort alist alist-entry-<))

(define (assert-distribution expected-alist got-distribution)
  (assert-equal (sort-alist expected-alist)
		(sort-alist (distribution->alist got-distribution))))

(define ((assert-prob-converges dist indeterminacy-bound)
	 datum expected-prob)
  (distribution/refine-to-mass-bound! dist indeterminacy-bound)
  (assert-in-delta expected-prob
		   (distribution/datum-min-probability dist datum)
		   indeterminacy-bound)
  (assert-in-delta expected-prob
		   (distribution/datum-max-probability dist datum)
		   indeterminacy-bound))

(define (assert-fully-forced distribution)
  (assert-false (distribution/refine! distribution))
  (assert-= (distribution/min-normalizer distribution)
	    (distribution/max-normalizer distribution)))
