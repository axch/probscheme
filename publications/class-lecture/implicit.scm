;;;; The probability language

(define-syntax discrete-select
  (syntax-rules ()
    ((DISCRETE-SELECT (expression mass) ...)
     ((CHOOSE (STREAM (MAKE-POSSIBILITY (LAMBDA () expression) mass)
                      ...))))))

(define (choose choice-stream)
  (call-with-current-continuation
    (lambda (continuation)
      (let ((schedule (current-schedule)))
        (schedule/choose schedule continuation choice-stream)
        (schedule/continue schedule)))))

(define (distribution-select distribution)
  (choose (distribution->mass-stream distribution)))

(define (observe! condition)
  (if (not condition)
      ((current-impossibility-continuation)
       (make-impossibility (schedule/reach-density (current-schedule))))))

;;;; Managing choice piles

(define-structure (choice-state
                   (constructor make-choice-state
                                (continuation choice-stream reach-density))
                   (conc-name choice-state/))
  (continuation #f read-only #t)
  choice-stream
  (reach-density #f read-only #t))

(define (schedule/choose schedule continuation choice-stream)
  (schedule/add! schedule
    (make-choice-state
     continuation choice-stream (schedule/reach-density schedule))))

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
              (schedule/continue/choice schedule choice-state choice))))))

;;;; Managing choice piles continued

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
      ((current-impossibility-continuation) impossibility))))

;;;; Turning thunks into distributions

(define (for-each-probability
	 procedure stochastic-thunk #!optional schedule-constructor)
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
             (make-possibility
	      datum (schedule/reach-density (current-schedule)))))))
      (schedule/continue (current-schedule)))))

(define stochastic-thunk->density-stream
  (walker->streamer for-each-probability))

(define (stochastic-thunk->distribution thunk #!optional schedule-constructor)
  (stream->distribution
   (stochastic-thunk->density-stream thunk schedule-constructor)))

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

;;; Walker->streamer

;;; Given a WALKER procedure with signature (WALKER PROCEDURE . ARGS)
;;; that calls PROCEDURE over all elements of some collection
;;; (presumably determined by the ARGS), returns a streamer procedure
;;; with signature ((WALKER->STREAMER WALKER) . ARGS) that returns a
;;; stream of all the values the WALKER would have passed to its
;;; PROCEDURE.  This is done by passing WALKER a carefully crafted
;;; procedure that uses continuations to invert control.

(define (walker->streamer walker)
  (lambda walker-args
    (lazy
     (call-with-current-continuation
      (lambda (k-main)
	(apply
	 walker
	 (cons
	  (lambda (value)
	    (call-with-current-continuation
	     (lambda (k-reenter)
	       (k-main (stream-cons value
				    (lazy
				     (call-with-current-continuation
				      (lambda (k-new-main)
					(set! k-main k-new-main)
					(k-reenter #f)))))))))
	  walker-args))
	(k-main stream-nil))))))
