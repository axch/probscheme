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

;;;; Probability Language
;;;; Implicit Control Structures

(declare (usual-integrations))

;;;; Schedules

;;; The schedule maintains the collection of as-yet unexplored options
;;; in as-yet not completely explored choice-states (below).  Besides
;;; that, it maintains the density for reaching the current point in
;;; the code.

(define-structure (schedule
                   (constructor make-schedule
                                (top-level
                                 content
                                 operation/empty?
                                 operation/current
                                 operation/add!
                                 operation/remove!))
                   (conc-name schedule/))
  (top-level #f read-only #t)
  (reach-density 1)
  (content #f read-only #t)
  (operation/empty? #f read-only #t)
  (operation/current #f read-only #t)
  (operation/add! #f read-only #t)
  (operation/remove! #f read-only #t))

(define (schedule/empty? schedule)
  ((schedule/operation/empty? schedule) (schedule/content schedule)))

(define (schedule/current schedule)
  ((schedule/operation/current schedule) (schedule/content schedule)))

(define (schedule/add! schedule item)
  ((schedule/operation/add! schedule) (schedule/content schedule) item))

(define (schedule/remove! schedule)
  ((schedule/operation/remove! schedule) (schedule/content schedule)))

;;; The relevant things about a choice-state are the location in the
;;; execution where it occurs (the continuation), the collection of
;;; options still available and not yet attempted in this choice (the
;;; choice-stream), and the probability of reaching this choice point
;;; (the reach-density).  This last does not take any observations
;;; occurring on other branches into account, and is consequently not
;;; normalized (thus named a density).

(define-structure (choice-state
                   (constructor make-choice-state
                                (continuation choice-stream reach-density))
                   (conc-name choice-state/))
  (continuation #f read-only #t)
  choice-stream
  (reach-density #f read-only #t))

(define (schedule/choose schedule continuation choice-stream)
  (schedule/add! schedule
                 (make-choice-state continuation
                                    choice-stream
                                    (schedule/reach-density schedule))))

;;;;; Continuing the Schedule

(define (schedule/continue schedule)
  (if (schedule/empty? schedule)
      ((schedule/top-level schedule) unspecific)
      (let* ((choice-state (schedule/current schedule))
             (choice-stream (choice-state/choice-stream choice-state)))
        (if (not (stream-pair? choice-stream))
            (begin (schedule/remove! schedule)
                   (schedule/continue schedule))
            (let ((choice (stream-car choice-stream)))
              (set-choice-state/choice-stream! choice-state
                                               (stream-cdr choice-stream))
              (schedule/continue/choice schedule
                                        choice-state
                                        choice))))))

(define (schedule/continue/choice schedule choice-state choice)
  (cond ((possibility? choice)
         (continue/possibility schedule choice-state choice))
        ((impossibility? choice)
         (continue/impossibility schedule choice-state choice))
        (else
         (error "Invalid element in possibility stream:" choice))))

(define (continue/possibility schedule choice-state possibility)
  (set-schedule/reach-density!
   schedule
   (* (possibility/density possibility)
      (choice-state/reach-density choice-state)))
  ((choice-state/continuation choice-state)
   (possibility/datum possibility)))

(define (continue/impossibility schedule choice-state impossibility)
  schedule                              ;ignore
  (within-continuation (choice-state/continuation choice-state)
    (lambda ()
      ((current-impossibility-continuation)
       (make-impossibility (* (impossibility/density impossibility)
			      (choice-state/reach-density choice-state)))))))

;;;;; Schedules of Various Sorts

(define (make-schedule-constructor make-content empty? current add! remove!)
  (lambda (top-level)
    (make-schedule top-level (make-content) empty? current add! remove!)))

(define make-depth-first-schedule
  (make-schedule-constructor make-stack
                             stack-empty?
                             stack-top
                             stack-push!
                             stack-pop!))

(define make-breadth-first-schedule
  (make-schedule-constructor make-queue
                             queue-empty?
                             queue-first
                             enqueue!
                             dequeue!))

(define make-priority-schedule
  (make-schedule-constructor
   make-priority-queue
   priority-queue-empty?
   priority-queue-first
   priority-enqueue!
   priority-dequeue!))

;;;; Dynamic State

(define *schedule*)
(define *impossibility-continuation*)

(define (current-schedule) *schedule*)
(define (current-impossibility-continuation) *impossibility-continuation*)

(define (with-schedule constructor thunk)
  (call-with-current-continuation
    (lambda (top-level)
      (fluid-let ((*schedule* (constructor top-level)))
        (thunk)))))

(define (with-impossibility-continuation thunk)
  (call-with-current-continuation
    (lambda (impossibility-continuation)
      (fluid-let ((*impossibility-continuation* impossibility-continuation))
        (thunk)))))

;++ This could stand a better name.

(define (for-each-probability procedure
                              stochastic-thunk
                              #!optional schedule-constructor)
  (with-schedule (if (default-object? schedule-constructor)
                     make-depth-first-schedule
                     schedule-constructor)
    (lambda ()
      (procedure
       (with-impossibility-continuation
         (lambda ()
           (let ((datum (stochastic-thunk)))
             ;** Do not beta-reduce this; we must call the thunk *before*
             ;** fetching the current reach density.
             (make-possibility datum
                               (schedule/reach-density (current-schedule)))))))
      (schedule/continue (current-schedule)))))

(define stochastic-thunk->density-stream (walker->streamer for-each-probability))

(define (stochastic-thunk->distribution thunk #!optional schedule-constructor)
  (stream->distribution
   (stochastic-thunk->density-stream thunk schedule-constructor)))

;;;; Probabilistic Language

(define-syntax discrete-select
  (syntax-rules ()
    ((DISCRETE-SELECT (expression mass) ...)
     ((CHOOSE (STREAM (MAKE-POSSIBILITY (LAMBDA () expression)
                                        mass)
                      ...))))))

;; Unlike distribution-splice, below, this normalizes the argument
;; distribution.
(define (distribution-select distribution)
  (choose (distribution->mass-stream distribution)))

;; Unlike distribution-select, above, this passes any disappeared
;; density up into the caller.
(define (distribution-splice distribution)
  (choose (distribution->density-stream distribution)))

(define (choose choice-stream)
  (call-with-current-continuation
    (lambda (continuation)
      (let ((schedule (current-schedule)))
        (schedule/choose schedule continuation choice-stream)
        (schedule/continue schedule)))))

(define (observe! condition)
  (if (not condition)
      ((current-impossibility-continuation)
       (make-impossibility (schedule/reach-density (current-schedule))))))

;;; The idea of a likelihood is useful only as a shortcut.  If you
;;; are about to embark on some generation of some probability
;;; distribution, and you know that the only thing you will do with
;;; that distribution is OBSERVE! that something is true of it, and
;;; you already know what the probability that this observation will
;;; turn out to be true is, then you can short-cut the whole process
;;; and just invoke LIKELIHOOD!.  It is very much worth understanding
;;; that this LIKELIHOOD! thing is not fundamental to the idea of 
;;; inference.
;;;
;;; I wonder whether the dependency discoverer can be made smart
;;; enough to come up with this kind of optimization on its own.

(define (likelihood! mass)
  (observe! (discrete-select (#t mass) (#f (- 1 mass)))))
