(define (discrete-select . options)
  (call-with-current-continuation
   (lambda (return)
     (choice-stack-push!
      *choice-stack* 
      (make-selection-cell
       return options
       (ps/running-prior *choice-stack*)
       (ps/running-likelihood *choice-stack*)))
     (choice-try! *choice-stack*))))

(define (likelihood! number)
  (set-ps/running-likelihood! 
   *choice-stack*
   (* number (ps/running-likelihood *choice-stack*))))

(define (observe! event)
  (if event
      'ok ; or, what's the same thing, (likelihood! 1)
      (likelihood! 0)))
