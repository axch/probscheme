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

;; This is a reimplementation of the Theory-Based Bayes model from
;; Theory-based induction. Kemp, C. S. and Tenenbaum,
;; J. B. (2003). Proceedings of the Twenty-Fifth Annual Conference of
;; the Cognitive Science Society.

;; It differs from animal-tree.scm in that I use explicit probability
;; distributions instead of implicit ones, to display what implicit
;; distributions do.
(define *kemp-tenenbaum-tree*
  '(0 (1/4 (1/4 (1/2 (0 chimp) (0 gorilla))
		(1/6 (1/6 (1/6 horse) (1/6 cow))
		     (1/6 (1/6 elephant) (1/6 rhino))))
	   (3/4 (0 mouse) (0 squirrel)))
      (5/6 (1/6 dolphin) (1/6 seal))))

(define (tree-lead-length tree) (car tree))
(define (tree-terminal? tree) (symbol? (cadr tree)))
(define (tree-terminal tree) (cadr tree))
(define (tree-left-branch tree) (cadr tree))
(define (tree-right-branch tree) (caddr tree))

(define (turn-on-probability lambda branch-length)
  (- 1 (exp (* -1 lambda branch-length))))

(define ((concept-prior mutation-rate) tree)
  (define (merge-subtrees tree already-on?)
    (independent-product (walk (tree-left-branch tree) already-on?)
			 (walk (tree-right-branch tree) already-on?)
			 (lambda (left-concept right-concept)
			   (append left-concept right-concept))))
  (define (walk tree already-on?)
    (let ((on-prob (turn-on-probability mutation-rate
					(tree-lead-length tree))))
      (if already-on?
	  (if (tree-terminal? tree)
	      (make-discrete-distribution `((,(tree-terminal tree)) 1))
	      (merge-subtrees tree #t))
	  (if (tree-terminal? tree)
	      (make-discrete-distribution
	       `(() ,(- 1 on-prob))
	       `((,(tree-terminal tree)) ,on-prob))
	    (mixture
	     (list (merge-subtrees tree #t) on-prob)
	     (list (merge-subtrees tree #f) (- 1 on-prob)))))))
  (walk tree #f))

(define (posterior-concept-dist assumptions)
  (conditional-distribution
   ((concept-prior 0.1) *kemp-tenenbaum-tree*)
   (lambda (concept)
     (reduce boolean/and #t (map (lambda (assumption)
				   (memq assumption concept))
				 assumptions)))))

(define (specific-argument-strength assumptions conclusion)
  (truth-probability (posterior-concept-dist assumptions)
		     (lambda (concept)
		       (memq conclusion concept))))

(define (general-argument-strength assumptions)
  (truth-probability (posterior-concept-dist assumptions)
		     (lambda (concept)
		       (equal? (sort concept symbol<?)
			       '(chimp cow dolphin elephant gorilla 
				 horse mouse rhino seal squirrel)))))
