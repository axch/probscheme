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

;; TODO Find a permanent solution to the library distribution
;; problem.
(define (self-relatively thunk)
  (if (current-eval-unit #f)
      (with-working-directory-pathname
       (directory-namestring (current-load-pathname))
       thunk)
      (thunk)))

(define (load-relative filename)
  (self-relatively (lambda () (load filename))))

(set! load/suppress-loading-message? #t) (newline)

(load-relative "testing/load")

(load-relative "examples/coin-flipping")
(load-relative "examples/bernoulli-urn")
(load-relative "examples/animal-tree")

(load-relative "lazy-test")
(load-relative "explicit-distributions-test")
(load-relative "implicit-distributions-test")
(in-test-group
 examples
 (load-relative "examples/die-rolling-test")
 (load-relative "examples/bernoulli-urn-test")
 (load-relative "examples/drunken-sailor-test")
 (load-relative "examples/geometric-test")
 (load-relative "examples/normalization-test")
 (load-relative "examples/math-problems-test")
 (load-relative "examples/coin-flipping-test")
 (load-relative "examples/animal-tree-test"))

(define my-path (directory-namestring (current-load-pathname)))

;; This is twisted, but that's an artifact of lack of module system.
;; There does not currently seem to be any other way to run the same
;; tests against different implementations of the functions they test.
(let ((expl-group (make-test-group 'explicit-distributions)))
  (tg:register-test! expl-group 
		     (tg:get *current-test-group*
			     '(examples coin-flipping-test)))
  (tg:register-test! expl-group 
		     (tg:get *current-test-group*
			     '(examples animal-tree-test)))
  (set-tg:group-set-up! expl-group
			(lambda ()
			  (with-working-directory-pathname
                           my-path
                           (lambda ()
                             (load "examples/more-coin-flipping")
                             (load "examples/more-animal-tree")))))
  (register-test expl-group))

(in-test-group
 explicit-distributions
 (load-relative "examples/more-animal-tree-impl-test"))

(let ((mltea-group (make-test-group 'mltea)))
  (tg:register-test! mltea-group
		     (tg:get *current-test-group*
			     '(examples animal-tree-test)))
  (set-tg:group-set-up! mltea-group
			(lambda ()
			  (with-working-directory-pathname
                           my-path
                           (lambda ()
                             (load "publications/mltea_talk/load")))))
  (set-tg:group-tear-down! mltea-group
			   (lambda ()
                             (with-working-directory-pathname
                              my-path
                              (lambda ()
                                (load "load-probscheme")))))
  (register-test mltea-group))
