(define (with-probabilistic-choices thunk)
  (let ((value-pile '()))
    (fluid-let ((*choice-stack* (make-choice-stack)))
      (let* ((value (thunk))
	     (prob
	      (* (ps/running-prior *choice-stack*)
		 (ps/running-likelihood *choice-stack*))))
	(set! value-pile (cons (list value prob)
			       value-pile)))
      (if (not (choice-try! *choice-stack*))
	  (apply make-discrete-distribution
		 value-pile)))))

