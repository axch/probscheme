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

(define (make-priority-queue)
  (make-binary-heap (lambda (cs-a cs-b)
		      (< (choice-state/reach-density cs-a)
			 (choice-state/reach-density cs-b)))))

(define (priority-queue-empty? queue)
  (binary-heap:empty? queue))

(define (priority-queue-first queue)
  (heap-node:data (binary-heap:root queue)))

(define (priority-enqueue! queue choice-state)
  (binary-heap:add-node! queue (make-heap-node choice-state)))

(define (priority-dequeue! queue)
  (heap-node:data (binary-heap:remove-root! queue)))
