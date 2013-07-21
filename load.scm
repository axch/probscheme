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

(define (self-relatively thunk)
  (if (current-eval-unit #f)
      (with-working-directory-pathname
       (directory-namestring (current-load-pathname))
       thunk)
      (thunk)))

(define (load-relative filename)
  (self-relatively (lambda () (load filename))))

(load-relative "auto-compilation")

(load-relative-compiled "shift-reset")

;;; SRFI-45 provides an implementation of LAZY that uses iterative
;;; forcing, but being written in r5rs Scheme it does not take
;;; advantage of any MIT-Scheme optimizations that the naive LAZY
;;; does.  As a consequence, it appears to slow the system
;;; appreciably, even though it does resolve issues with stack
;;; overflow.

; (define-syntax lazy
;   (syntax-rules ()
;     ((LAZY promise)
;      (DELAY (FORCE promise)))))

(load-relative-compiled "srfi-45")

(load-relative-compiled "lazy")
(load-relative-compiled "stack")
(load-relative-compiled "queue")
(load-relative-compiled "binary-heap")
(load-relative-compiled "priority-queue")
(load-relative-compiled "possibility")
(load-relative-compiled "distribution")
(load-relative-compiled "implicit")
(load-relative-compiled "library")

(load-relative-compiled "test-helper")
