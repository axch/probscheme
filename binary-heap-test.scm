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

;;; Binary Heap test

(define (max-compare node1 node2)
  (> (heap-node:data node1)
     (heap-node:data node2)))

(define heap1
  (make-binary-heap max-compare))

(binary-heap:level-array heap1)
;Value: #(#(#f))

(binary-heap:empty? heap1)
;Value: #t

(define root-position
  (make-node-position 0 0))

(binary-heap:lookup heap1 root-position)
;Value: #f

(define node1
  (make-heap-node 15))

(begin 
  (pp (heap-node:position node1))
  (pp (heap-node:data node1)))
;No value

;; Output:
;#f
;15
;

(binary-heap:set-node! heap1 node1 root-position)

(binary-heap:lookup heap1 root-position)
;Value: #[heap-node 52]

(binary-heap:empty? heap1)
;Value: #f

(let ((root (binary-heap:lookup heap1 root-position)))
  (pp (heap-node:position root))
  (pp (heap-node:data root)))


;; Output:
;(0 . 0)
;15
;

(binary-heap:add-level! heap1)

(binary-heap:level-array heap1)
;Value: #(#(#[heap-node 69]) #(#f #f))

(binary-heap:add-level! heap1)
(binary-heap:add-level! heap1)

(binary-heap:level-array heap1)
;Value: #(#(#[heap-node 84]) #(#f #f) #(#f #f #f #f) #(#f #f #f #f #f #f #f #f))

(define pos2
  (make-node-position 1 0))

(binary-heap:set-node! heap1 node1 pos2)

(binary-heap:level-array heap1)
;Value: #(#(#f) #(#[heap-node 84] #f) #(#f #f #f #f) #(#f #f #f #f #f #f #f #f))

(successor-position root-position)
;Value: (1 . 0)

(successor-position (successor-position root-position))
;Value: (1 . 1)


;Value: done

;; Output:
;(0 . 0)
;(1 . 0)
;(1 . 1)
;(2 . 0)
;(2 . 1)
;(2 . 2)
;(2 . 3)
;(3 . 0)
;(3 . 1)
;(3 . 2)
;(3 . 3)
;(3 . 4)
;(3 . 5)
;(3 . 6)
;(3 . 7)
;(4 . 0)
;(4 . 1)
;(4 . 2)
;(4 . 3)
;(4 . 4)
;(4 . 5)
;

(let loop ((pos (make-node-position 4 6)) (n 0))
  (if (> n 20)
      'done
      (begin
	(pp pos)
	(loop (predecessor-position pos) (+ n 1)))))
;Value: done

;; Output:
;(4 . 6)
;(4 . 5)
;(4 . 4)
;(4 . 3)
;(4 . 2)
;(4 . 1)
;(4 . 0)
;(3 . 7)
;(3 . 6)
;(3 . 5)
;(3 . 4)
;(3 . 3)
;(3 . 2)
;(3 . 1)
;(3 . 0)
;(2 . 3)
;(2 . 2)
;(2 . 1)
;(2 . 0)
;(1 . 1)
;(1 . 0)
;

(define pos3
  (make-node-position 4 4))

(parent-position pos3)
;Value: (3 . 2)

(children-positions (parent-position pos3))
;Value: ((4 . 4) (4 . 5))

(define heap2
  (make-binary-heap max-compare))

(binary-heap:add-node! heap2 (make-heap-node 100))

(binary-heap:level-array heap2)
;Value: #(#(#[heap-node 110]) #(#f #f))

(define node-list
  (list (make-heap-node 19)
	(make-heap-node 36)
	(make-heap-node 17)
	(make-heap-node 3)
	(make-heap-node 25)
	(make-heap-node 1)
	(make-heap-node 2)
	(make-heap-node 7)))

(define (add-nodes-to-heap! heap node-list)
  (if (not (null? node-list))
      (begin
	(binary-heap:add-node! heap (car node-list))
	(add-nodes-to-heap! heap (cdr node-list)))))

(add-nodes-to-heap! heap2 node-list)

(binary-heap:level-array heap2)
;Value: #(#(#[heap-node 126]) #(#[heap-node 127] #[heap-node 128]) #(#[heap-node 129] #[heap-node 130] #[heap-node 131] #[heap-node 132]) #(#[heap-node 133] #[heap-node 134] #f #f #f #f #f #f))

(heap-node:data (binary-heap:root heap2))

(define (level-array->data heap)
  (vector-map (lambda (vector)
		 (vector-map (lambda (node)
			       (if node
				   (heap-node:data node)))
			     vector))
	       (binary-heap:level-array heap)))

(level-array->data heap2)

(define reverse-node-list
  (reverse
   (list (make-heap-node 19)
	 (make-heap-node 36)
	 (make-heap-node 17)
	 (make-heap-node 3)
	 (make-heap-node 25)
	 (make-heap-node 1)
	 (make-heap-node 2)
	 (make-heap-node 7))))

(map (lambda (node)
       (heap-node:position node))
     reverse-node-list)
;Value: (#f #f #f #f #f #f #f #f)

(define heap3
  (make-binary-heap max-compare))

(add-nodes-to-heap! heap3 reverse-node-list)

(level-array->data heap3)
;Value: #(#(36) #(19 25) #(7 3 1 17) #(2 #!unspecific #!unspecific #!unspecific #!unspecific #!unspecific #!unspecific #!unspecific))

(let* ((node (binary-heap:root heap3))
       (data (heap-node:data node))
       (position (heap-node:position node)))
  (pp data)
  (pp position))
;No value

;; Output:
;36
;(0 . 0)
;

(binary-heap:remove-root! heap3)

(let* ((node (binary-heap:root heap3))
       (data (heap-node:data node))
       (position (heap-node:position node)))
  (pp data)
  (pp position))
;No value

;; Output:
;25
;(0 . 0)
;

(level-array->data heap3)
;Value: #(#(25) #(19 17) #(7 3 1 2) #(#!unspecific #!unspecific #!unspecific #!unspecific #!unspecific #!unspecific #!unspecific #!unspecific))


(let loop ()
  (if (not (binary-heap:empty? heap3))
      (begin (pp (heap-node:data (binary-heap:root heap3)))
	     (pp (level-array->data heap3))
	     (binary-heap:remove-root! heap3)
	     (loop))))

;No value

;; Output:
;25
;#(#(25)
;  #(19 17)
;  #(7 3 1 2)
;  #(#!unspecific #!unspecific #!unspecific #!unspecific
;    #!unspecific #!unspecific #!unspecific #!unspecific))
;19
;#(#(19)
;  #(7 17)
;  #(2 3 1 #!unspecific)
;  #(#!unspecific #!unspecific #!unspecific #!unspecific
;    #!unspecific #!unspecific #!unspecific #!unspecific))
;17
;#(#(17)
;  #(7 1)
;  #(2 3 #!unspecific #!unspecific)
;  #(#!unspecific #!unspecific #!unspecific #!unspecific
;    #!unspecific #!unspecific #!unspecific #!unspecific))
;7
;#(#(7)
;  #(3 1)
;  #(2 #!unspecific #!unspecific #!unspecific)
;  #(#!unspecific #!unspecific #!unspecific #!unspecific
;    #!unspecific #!unspecific #!unspecific #!unspecific))
;3
;#(#(3)
;  #(2 1)
;  #(#!unspecific #!unspecific #!unspecific #!unspecific)
;  #(#!unspecific #!unspecific #!unspecific #!unspecific
;    #!unspecific #!unspecific #!unspecific #!unspecific))
;2
;#(#(2)
;  #(1 #!unspecific)
;  #(#!unspecific #!unspecific #!unspecific #!unspecific)
;  #(#!unspecific #!unspecific #!unspecific #!unspecific
;    #!unspecific #!unspecific #!unspecific #!unspecific))
;1
;#(#(1)
;  #(#!unspecific #!unspecific)
;  #(#!unspecific #!unspecific #!unspecific #!unspecific)
;  #(#!unspecific #!unspecific #!unspecific #!unspecific
;    #!unspecific #!unspecific #!unspecific #!unspecific))
;

