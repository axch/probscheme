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

(define condition-type:test-failure
  (make-condition-type 'test-failure condition-type:error
		       '(message) (lambda (condition port)
				    (display (access-condition condition 'message) port))))

(define condition/test-failure?
  (condition-predicate condition-type:test-failure))

(define test-fail
  (condition-signaller condition-type:test-failure
		       '(message) standard-error-handler))

(define (instantiate-template template arguments)
  (if (not (= (length arguments) (- (length template) 1)))
      (error "Template and argument lists are length-mismatched: "
	     template arguments))
  (let loop ((result (car template))
	     (template (cdr template))
	     (arguments arguments))
    (if (null? template)
	result
	(loop (string-append result (car arguments) (car template))
	      (cdr template)
	      (cdr arguments)))))

(define (messagify object)
  (with-output-to-string (lambda () (display object))))

(define (build-message header template . arguments)
  (let ((body (instantiate-template template (map messagify arguments))))
    (if header
	(string-append header "\n" body)
	(string-append "\n" body))))

(define (assert-proc message proc)
  (if (proc)
      'ok
      (test-fail message)))

(define (assert-equivalent predicate #!optional pred-name)
  (define (full-message message expected actual)
    (if (default-object? pred-name)
	(build-message message
		       '("<" "> expected but was\n<" ">.")
		       expected actual)
	(build-message message
		       '("<" "> expected to be " " to\n<" 
			 ">.")
		       expected pred-name actual)))
  (lambda (expected actual #!optional message)
    (if (default-object? message) (set! message #f))
    (assert-proc (full-message message expected actual)
		 (lambda () (predicate expected actual)))))

(define assert-eq (assert-equivalent eq? "eq?"))
(define assert-eqv (assert-equivalent eqv? "eqv?"))
(define assert-equal (assert-equivalent equal? "equal?"))
(define assert-= (assert-equivalent = "="))
(define assert-equals assert-equal)
(define assert= assert-=)

(define (assert-in-delta expected actual delta #!optional message)
  (if (default-object? message) (set! message #f))
  (let ((full-message
	 (build-message message '("<" "> and\n<" "> expected to be within\n<"
				  "> of each other.")
			expected actual delta)))
    (assert-proc full-message (lambda () (<= (abs (- expected actual)) delta)))))

(define (assert-matches regexp string #!optional message)
  (if (default-object? message) (set! message #f))
  (let ((full-message
	 (build-message message '("<" "> expected to match <" ">")
			string regexp)))
    (assert-proc full-message
		 (lambda ()
		   ;; TODO Fix this
		   (re-string-search-forward regexp string)))))

(define (assert-true thing #!optional message)
  (if (default-object? message) (set! message #f))
  (let ((full-message
	 (build-message message '("<" "> expected to be a true value.")
			thing)))
    (assert-proc full-message (lambda () thing))))

(define (assert-false thing #!optional message)
  (if (default-object? message) (set! message #f))
  (let ((full-message
	 (build-message message '("<" "> expected to be a false value.")
			thing)))
    (assert-proc full-message (lambda () (not thing)))))
