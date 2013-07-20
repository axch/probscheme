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

;;;; FIFO Queues

(define-structure (queue (constructor %make-queue))
  front
  rear)

(define (make-queue)
  (let ((cell (cons #f '())))
    (%make-queue cell cell)))

(define (queue-empty? queue)
  (eq? (queue-front queue)
       (queue-rear queue)))

(define (queue-first queue)
  (cadr (queue-front queue)))

(define (enqueue! queue element)
  (let ((tail (cons element '())))
    (set-cdr! (queue-rear queue) tail)
    (set-queue-rear! queue tail)))

(define (dequeue! queue)
  (set-queue-front! queue (cdr (queue-front queue)))
  ;; Destroy useless pointers.
  (set-car! (queue-front queue) #f))
