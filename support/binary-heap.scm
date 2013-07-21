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

;;; Binary Heap Infrastructure
;; 

(define-record-type <binary-heap>
    (%make-binary-heap max-compare
		       level-array
		       last-leaf-position)
    binary-heap?
  (level-array binary-heap:level-array
	       binary-heap:set-level-array!)
  (last-leaf-position binary-heap:last-leaf-position
	     binary-heap:set-last-leaf-position!)
  (max-compare binary-heap:max-compare
	       binary-heap:set-max-compare!))


(define (make-binary-heap max-compare)
  (let ((level-array
	 (make-vector 1 (make-vector 1 #f))))
    (%make-binary-heap max-compare level-array (make-node-position 0 0))))

;;; Heap Node

(define-record-type <heap-node>
    (make-heap-node data)
    heap-node?
  (data heap-node:data heap-node:set-data!)
  (node-position heap-node:position heap-node:set-position!))


;;; Node position
;; each node has a position in the heap, represented by its level and
;; offset. Level represents the node's depth in the tree.

(define (make-node-position level offset)
  (cons level offset))

(define (node-position:level node-position)
  (car node-position))

(define (node-position:offset node-position)
  (cdr node-position))


;; root is the first element in the first level, 0-indexed.
(define (binary-heap:root heap)
  (binary-heap:lookup heap (make-node-position 0 0)))

(define (binary-heap:empty? heap)
  (not (binary-heap:root heap)))

;; returns node or #f if node not found
(define (binary-heap:lookup binary-heap node-position)
  (let ((level (node-position:level node-position))
	(offset (node-position:offset node-position))
	(level-array (binary-heap:level-array binary-heap)))
    (if (> level (- (vector-length level-array) 1))
	#f 
	(vector-ref (vector-ref level-array level) offset))))

(define (%binary-heap:set-cell! binary-heap value node-position)
  (let ((level (node-position:level node-position))
	(offset (node-position:offset node-position))
	(level-array (binary-heap:level-array binary-heap)))
    (vector-set! (vector-ref level-array level) offset value)))

;; returns #f is node doesn't have a position
(define (binary-heap:in-advertised-cell? binary-heap node)
  (let ((position (heap-node:position node)))
    (and position
	 (eq? (binary-heap:lookup binary-heap position)
	      node))))

(define (binary-heap:detach-node! binary-heap node)
  (if (binary-heap:in-advertised-cell? binary-heap node)
      (%binary-heap:set-cell! binary-heap #f (heap-node:position node))))

;; unsets node from previous position and moves node to new position.
(define (binary-heap:set-node! binary-heap node node-position)
  (binary-heap:detach-node! binary-heap node)
  (%binary-heap:set-cell! binary-heap node node-position)
  (heap-node:set-position! node node-position))

(define (binary-heap:swap-nodes! binary-heap node1 node2)
  (let ((position1 (heap-node:position node1))
	(position2 (heap-node:position node2)))
    (binary-heap:set-node! binary-heap node1 position2)
    (binary-heap:set-node! binary-heap node2 position1)))
		

;; returns last element in a vector
(define (vector-last vector)
  (vector-ref vector (- (vector-length vector) 1)))

;; extends the level-array with a new level.
;; level 1 (root) has size 1 = 2^0
;; 2, 4, 8... level sizes
;; note: this does not change the last-leaf-position pointer to point
;; to anywhere in the new level!

(define (binary-heap:add-level! binary-heap)
  (let* ((level-array (binary-heap:level-array binary-heap))
	 (length (vector-length level-array))
	 (new-level-size (expt 2 length))
	 (new-level-array (vector-grow level-array (+ length 1))))
    (vector-set! new-level-array length (make-vector new-level-size #f))
    (binary-heap:set-level-array! binary-heap new-level-array)))


(define (level-full? node-position)
  (let* ((level (node-position:level node-position))
	(offset (node-position:offset node-position))
	(level-size (expt 2 level)))
    (= offset (- level-size 1))))	; because array is 0-indexed


;;; Position Calculations
(define (successor-position node-position)
  (let ((level (node-position:level node-position))
	(offset (node-position:offset node-position)))
    (if (level-full? node-position)
	(make-node-position (+ level 1) 0)
	(make-node-position level (+ offset 1)))))

(define (predecessor-position node-position)
  (let ((level (node-position:level node-position))
	(offset (node-position:offset node-position)))
    (if (and (= level 0) (= offset 0))
	(error "root has no predecessor")
	(if (= offset 0)
	    (make-node-position (- level 1)
				(- (expt 2 (- level 1)) 1))
	    (make-node-position level
				(- offset 1))))))
  
(define (parent-position node-position)
  (let ((level (node-position:level node-position))
	(offset (node-position:offset node-position)))
    (let ((parent-level (- level 1))
	  (parent-offset (floor (/ offset 2))))
      (make-node-position parent-level parent-offset))))

;; returns a list of children positions, whether the node has children
;; or not. 
(define (children-positions parent-position)
  (let ((parent-level (node-position:level parent-position))
	(parent-offset (node-position:offset parent-position)))
    (let ((children-level (+ parent-level 1))
	  (child1-offset (* 2 parent-offset))
	  (child2-offset (+ (* 2 parent-offset) 1)))
      (list (make-node-position children-level child1-offset)
	    (make-node-position children-level child2-offset)))))

;;; Parents and Children
;; looks up heap-node's parent in binary-heap
(define (heap-node:parent binary-heap heap-node)
  (let* ((node-position (heap-node:position heap-node))
	 (parent-position (parent-position node-position)))
    (binary-heap:lookup binary-heap parent-position)))

;; returns a list of children... if a child does not exists, then it
;; is represented as #f
(define (heap-node:children binary-heap parent-node)
  (let* ((parent-position (heap-node:position parent-node))
	 (children-positions (children-positions parent-position)))
    (filter-map (lambda (position)
		  (binary-heap:lookup binary-heap position))
		children-positions)))

;-------------------------------------------------------------------------------
;;; Heap Operations

;; checks that last-leaf position is empty, inserts a node in the
;; last-leaf position, updates last-leaf position, and adds a new
;; level to the binary-heap if necessary

(define (binary-heap:insert-last-leaf! binary-heap node)
  (let ((last-leaf-position (binary-heap:last-leaf-position binary-heap)))
    (if (level-full? last-leaf-position)
	(binary-heap:add-level! binary-heap))
    (let ((old-node (binary-heap:lookup binary-heap last-leaf-position)))
      (if old-node
	  (error "last-leaf position already contains a node:" old-node)))
    (binary-heap:set-node! binary-heap node last-leaf-position)
    (binary-heap:set-last-leaf-position! binary-heap
					 (successor-position last-leaf-position))))
	
(define (max-elt max-compare elts)
  (if (null? elts)
      (error "max-elt needs elements to select max from")
      (let loop ((max (car elts)) (elts (cdr elts)))
	(if (null? elts) max
	    (loop (if (max-compare (car elts) max)
		      (car elts)
		      max)
		  (cdr elts))))))

;; heapifies one node and children
(define (heapify-one-node! binary-heap node)
  (let ((children (heap-node:children binary-heap node))
	(max-compare (binary-heap:max-compare binary-heap)))
    (let ((max-node (max-elt max-compare (cons node children))))
      (binary-heap:swap-nodes! binary-heap node max-node))))


;; heapify along descendents, until we reach leaves.
(define (heapify-node-down! binary-heap node)
  (let ((children (heap-node:children binary-heap node)))
    (if (not (null? children))
	(let ((max-compare (binary-heap:max-compare binary-heap)))
	  (let ((max-node (max-elt max-compare (cons node children))))
	    (if (not (eq? node max-node))
		(begin
		  (binary-heap:swap-nodes! binary-heap node max-node)
		  (heapify-node-down! binary-heap node))))))))

;; because nodes are inserted at the leaf-level, we heapify up along
;; ancestors, until we reach the root.
;; All nodes, except the root, have a parent.

(define (heapify-node-up! binary-heap node)
  (if (not (eq? (binary-heap:root binary-heap) node))		    
      (let ((parent (heap-node:parent binary-heap node))
	    (max-compare (binary-heap:max-compare binary-heap)))
	(if (not (max-compare parent node))
	    (begin
	      (binary-heap:swap-nodes! binary-heap parent node)
	      (heapify-node-up! binary-heap node))))))

(define (binary-heap:add-node! binary-heap node)
  (binary-heap:insert-last-leaf! binary-heap node)
  (heapify-node-up! binary-heap node))

;; remove-node! swaps node with the last element (leaf) in the heap
;; backs the last-node pointer up 
;; returns the removed node.
(define (binary-heap:remove-node! binary-heap node)
  (let* ((new-last-leaf-position
	  (predecessor-position
	   (binary-heap:last-leaf-position binary-heap)))
	 (last-node (binary-heap:lookup
		     binary-heap
		     new-last-leaf-position)))
    (binary-heap:swap-nodes! binary-heap last-node node)
    (binary-heap:detach-node! binary-heap node)
    (binary-heap:set-last-leaf-position! binary-heap new-last-leaf-position)
    (heapify-node-down! binary-heap last-node)
    node))

(define (binary-heap:remove-root! binary-heap)
  (binary-heap:remove-node! binary-heap (binary-heap:root binary-heap)))