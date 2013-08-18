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

;; The general idea is that observed features of species may turn on
;; randomly at any point in a species' ancestry, leading to species
;; closer on the evolutionary tree to have more features in common.

;; A tree is a list of the leading branch length, and either
;; the terminal symbol or the left and right subtrees
;; The tree given in Figure 1 of that paper looks roughly like this.
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

(define (boolean-select prob-true)
  (discrete-select (#t prob-true) (#f (- 1 prob-true))))

;; These two are the concept generation as described in the paper
(define (turn-concept-on? mutation-rate branch-length)
  (and (> branch-length 0) ; A little optimization for fewer decisions
       (boolean-select (- 1 (exp (* -1 mutation-rate branch-length))))))

(define ((generate-concept mutation-rate) tree)
  (let walk ((concept-on #f)
	     (tree tree))
    (if (or concept-on (turn-concept-on? mutation-rate (tree-lead-length tree)))
	(if (tree-terminal? tree)
	    (list (tree-terminal tree))
	    (append (walk #t (tree-left-branch tree))
		    (walk #t (tree-right-branch tree))))
	(if (tree-terminal? tree)
	    '()
	    (append (walk #f (tree-left-branch tree))
		    (walk #f (tree-right-branch tree)))))))

(define (concept-containing animals)
  (let ((concept ((generate-concept 0.1) *kemp-tenenbaum-tree*)))
    (for-each (lambda (animal) (observe! (memq animal concept))) animals)
    concept))

;; This implements the inference about argument strengths of Osheron
;; specific arguments (with a hard-coded mutation-rate and a fixed tree).
;; The assumptions and the conclusion are species at leaves of the
;; tree.  The assumptions are assumed to be in the concept, and the
;; conclusion is the thing whose membership is to be tested.
(define (specific-argument-strength assumptions conclusion)
  (probability-of
   (stochastic-thunk->distribution
    (lambda ()
      (and (memq conclusion (concept-containing assumptions)) #t)))
   #t))

;; This implements the analagous inference about argument strengths of
;; Osheron general arguments.
(define (general-argument-strength assumptions)
  (probability-of
   (stochastic-thunk->distribution
    (lambda ()
      (equal? (sort (concept-containing assumptions) symbol<?)
	      '(chimp cow dolphin elephant gorilla 
		horse mouse rhino seal squirrel))))
   #t))
