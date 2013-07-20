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
 lazy-test

 (define-test (iterative-forcing)
   (define (integers start)
     (stream-cons start (integers (+ start 1))))

   (define (count-rejector count)
     (lambda (ignore)
       (set! count (- count 1))
       (<= count 0)))

   ;; If forcing is not iterative, this exceeds the recursion depth
   ;; limit of MIT Scheme on 100000
   (map (lambda (distance)
	  (assert-= distance (stream-car
			      (stream-filter (count-rejector distance)
					     (integers 1)))))
	'(1 10 100 1000 10000 100000)))

 (define-test (walker-streamer-test)
   (let ((ans-stream
	  ((walker->streamer for-each)
	   '(1 2 3 4 5 6))))
     (assert-true (stream-pair? ans-stream))
     (assert-equal '(1 2 3 4 5 6) (stream->list ans-stream))))

 (define-test (shift-reset-memory-test)
   (define (walk-integers procedure start stop)
     (if (>= start stop)
	 'done
	 (begin
	   (procedure (car (cons start stop)))
	   (walk-integers procedure (+ 1 start) stop))))

   (define (assert-stream-of-integers stream start)
     (if (stream-null? stream)
	 'ok
	 (begin
	   (assert-= (stream-car stream) start)
	   (assert-stream-of-integers (stream-cdr stream) (+ 1 start)))))

   ;; This takes too long to actually run a real memory stress test,
   ;; but one can manually extend this to something that would verify
   ;; the memory consumption of shift and reset by themselves.
   (for-each (lambda (limit)
	       (assert-stream-of-integers
		((walker->streamer walk-integers)
		 0 limit)
		0))
	     '(1 10 100 1000))))
