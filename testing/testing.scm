;;; ----------------------------------------------------------------------
;;; Copyright 2007 Alexey Radul.
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

;;;; Test Registration

(define (register-test test)
  (tg:register-test! *current-test-group* test))

;; TODO Teach Emacs to syntax-highlight this just like define
(define-syntax define-test
  (syntax-rules ()
    ((define-test (name formal ...) body-exp ...)
     (let ((proc (lambda (formal ...) body-exp ...)))
       (register-test (make-single-test 'name proc))))))

;;;; Test Running

;; Poor man's dynamic dispatch by storing the
;; procedures that do the job in a record
(define (run-given-test test-runner test)
  ((tr:run-one test-runner) (list (st:name test)) test))

(define (run-given-group test-runner group name-stack)
  ((tr:run-group test-runner) group name-stack))

(define (report-results test-runner)
  ((tr:report-results test-runner)))

(define (run-test test-name-stack #!optional test-runner)
  (if (default-object? test-runner)
      (set! test-runner (make-standard-test-runner)))
  (let ((test (tg:get (current-test-group) test-name-stack)))
    (cond ((test-group? test)
	   (run-given-group test-runner test '()))
	  ((single-test? test)
	   (run-given-test test-runner test))
	  (else
	   (error "Unknown test type" test)))
    (report-results test-runner)))

(define (run-registered-tests #!optional test-runner)
  (run-test '() test-runner))
