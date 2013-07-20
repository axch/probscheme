(define (choice-try! choice-stack)
  (if (choice-stack-empty? choice-stack)
      #f
      (let ((cell (top-cell choice-stack))
	    (continuation (top-continuation choice-stack))
	    (options (top-option-list choice-stack))
	    (prior (top-prior-mass choice-stack))
	    (likelihood (top-likelihood choice-stack)))
	(choice-stack-pop! choice-stack)
	(if (null? options)
	    (choice-try! choice-stack)
	    (begin (choice-stack-push!
		    choice-stack 
		    (cell-cdr-options cell))
		   (set-ps/running-prior! choice-stack
		    (* prior (next-option-prior options)))
		   (set-ps/running-likelihood!
		    choice-stack likelihood)
		   (continuation
		    (next-option-value options)))))))
