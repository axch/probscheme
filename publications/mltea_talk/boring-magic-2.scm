(define-structure 
  (choice-stack (constructor make-choice-stack ())
		(conc-name ps/))
  (stack '())
  (running-likelihood 1)
  (running-prior 1))

(define (choice-stack-push! choice-stack cell)
  (set-ps/stack! choice-stack
		 (cons cell (ps/stack choice-stack))))

(define (choice-stack-empty? choice-stack)
  (null? (ps/stack choice-stack)))

(define (top-cell choice-stack)
  (car (ps/stack choice-stack)))

(define (top-continuation choice-stack)
  (cell/continuation (car (ps/stack choice-stack))))

(define (top-option-list choice-stack)
  (cell/option-list (car (ps/stack choice-stack))))

(define (top-prior-mass choice-stack)
  (cell/prior-mass (car (ps/stack choice-stack))))

(define (top-likelihood choice-stack)
  (cell/likelihood (car (ps/stack choice-stack))))

(define (choice-stack-pop! choice-stack)
  (set-ps/stack! choice-stack
		 (cdr (ps/stack choice-stack))))

(define *choice-stack*)
