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

;;;; Laziness
;;;; Iterative Promises and Even Streams

(declare (usual-integrations))

(define-syntax stream-cons
  (syntax-rules ()
    ((STREAM-CONS a d)
     (DELAY (CONS a d)))))

(define (stream-pair? stream) (pair? (force stream)))
(define (stream-null? stream) (null? (force stream)))

(define stream-nil (delay '()))

(define (stream-car stream) (car (force stream)))
(define (stream-cdr stream) (cdr (force stream)))

(define (stream-map procedure stream)
  (lazy
   (if (stream-pair? stream)
       (stream-cons (procedure (stream-car stream))
                    (stream-map procedure (stream-cdr stream)))
       stream-nil)))

(define (stream-filter predicate stream)
  (let recur ((stream stream))
    (lazy
     (if (stream-pair? stream)
         (let ((item (stream-car stream))
               (recur (lambda () (recur (stream-cdr stream)))))
           (if (predicate item)
               (stream-cons item (recur))
               (recur)))
         stream-nil))))

(define (stream-filter-map procedure stream)
  (let recur ((stream stream))
    (lazy
     (if (stream-pair? stream)
         (let ((item (stream-car stream))
               (recur (lambda () (recur (stream-cdr stream)))))
           (cond ((procedure item)
                  => (lambda (item*)
                       (stream-cons item* (recur))))
                 (else (recur))))
         stream-nil))))

(define (stream-append stream-a stream-b)
  (let recur ((stream-a stream-a))
    (lazy
     (if (stream-pair? stream-a)
         (stream-cons (stream-car stream-a)
                      (recur (stream-cdr stream-a)))
         stream-b))))

(define (stream-diagonalize streams)
  (let diagonalize ((streams streams))
    (lazy
     (if (not (stream-pair? streams))
         stream-nil
         (let ((stream (stream-car streams)))
           (if (not (stream-pair? stream))
               (recur (stream-cdr streams))
               (stream-cons
                (stream-car stream)
                (let recur ((stream-a (diagonalize (stream-cdr streams)))
                            (stream-b (stream-cdr stream)))
                  (lazy
                   (if (stream-pair? stream-a)
                       (stream-cons (stream-car stream-a)
                                    (recur stream-b (stream-cdr stream-a)))
                       stream-b))))))))))

(define (list->stream list)
  (let recur ((list list))
    (lazy
     (if (pair? list)
         (stream-cons (car list) (recur (cdr list)))
         stream-nil))))

(define (stream->list stream)
  (let loop ((stream stream) (reversed-list '()))
    (if (stream-pair? stream)
        (loop (stream-cdr stream)
              (cons (stream-car stream) reversed-list))
        (reverse! reversed-list))))

(define-syntax stream
  (syntax-rules ()
    ((STREAM) STREAM-NIL)
    ((STREAM x y ...) (STREAM-CONS x (STREAM y ...)))))

;;; Given a walker procedure with signature (walker procedure . args)
;;; that calls procedure over all elements of some collection
;;; (presumably determined by the args), returns a streamer procedure
;;; with signature ((walker->streamer walker) . args) that returns a
;;; stream of all the values the walker would have passed to its
;;; procedure.  This is done by passing walker a carefully crafted
;;; procedure that uses shift/reset to invert control.
(define (walker->streamer walker)
  (lambda walker-args
    (reset
     (begin
       (apply
	walker
	(cons
	 (lambda (value)
	   (shift next-value
		  (stream-cons value (lazy (next-value unspecific)))))
	 walker-args))
       stream-nil))))

