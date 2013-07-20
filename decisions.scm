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

(define (hash-table/increment! hash-table key amount default)
  (hash-table/put! hash-table key
		   (+ amount (hash-table/get hash-table key default))))

(define-structure
  (decision-cache
   (constructor 
    %make-decision-cache
    (cache min-loss max-loss loss-function)))
  (cache #f read-only #t)
  (min-loss 0 read-only #t)
  (max-loss 0 read-only #t)
  (undetermined-density 1)
  (loss-function #f read-only #t))

(define (make-decision-cache options min-loss max-loss loss-function)
  (%make-decision-cache
   (alist->hash-table (map (lambda (opt)
			     (cons opt 0))
			   options))
   min-loss
   max-loss
   loss-function))

(define (cache-register! cache thing)
  (if (possibility? thing)
      (register-possbility! cache thing)
      (register-impossibility! cache thing)))

(define (register-possbility! cache possibility)
  (for-each 
   (lambda (option)
     (let ((option-loss (* (possibility/density possibility)
			   ((decision-cache-loss-function cache)
			    option (possibility/datum possibility)))))
       (hash-table/increment! 
	(decision-cache-cache cache) option option-loss 0)))
   (hash-table/key-list (decision-cache-cache cache)))
  (set-decision-cache-undetermined-density! 
   cache (- (decision-cache-undetermined-density cache) 
	    (possibility/density possibility))))

(define (register-impossibility! cache impossibility)
  (set-decision-cache-undetermined-density! 
   cache (- (decision-cache-undetermined-density cache) 
	    (impossibility/density impossibility))))

(define (best-case-loss cache current-loss)
  (+ (* (decision-cache-undetermined-density cache)
	(decision-cache-min-loss cache))
     current-loss))

(define (worst-case-loss cache current-loss)
  (+ (* (decision-cache-undetermined-density cache)
	(decision-cache-max-loss cache))
     current-loss))

(define (sorted-choice-alist cache)
  (sort (hash-table->alist (decision-cache-cache cache))
	(lambda (option-1 option-2)
	  (< (cdr option-1) (cdr option-2)))))

(define (best-choice cache #!optional error-tolerance)
  (if (default-object? error-tolerance) (set! error-tolerance 0))
  (let ((choices (sorted-choice-alist cache)))
    (if (= 1 (length choices))
	(caar choices)
	(if (< (worst-case-loss cache (cdar choices))
	       (+ (best-case-loss cache (cdadr choices))
		  error-tolerance))
	    (caar choices)
	    #f))))

(define (decide options knowledge loss-function min-loss max-loss
		#!optional error-tolerance)
  (let ((cache (make-decision-cache options min-loss max-loss loss-function))
	(possibility-count 0))
    (let loop ((knowledge-stream (distribution->density-stream knowledge)))
      (let ((winner (best-choice cache error-tolerance)))
	(if winner
	    (begin 
	      (display "considered ")
	      (display possibility-count)
	      (display " possibilities")
	      (newline)
	      winner)
	    (if (stream-null? knowledge-stream)
		#f ; TODO what if the knowledge stream ends?
		(begin (if (possibility? (stream-car knowledge-stream))
			   (set! possibility-count (+ possibility-count 1)))
		       (cache-register! cache (stream-car knowledge-stream))
		       (loop (stream-cdr knowledge-stream)))))))))
