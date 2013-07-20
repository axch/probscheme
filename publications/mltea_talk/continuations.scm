1 ]=> (+ (* 3 4) 5)
;Value: 17

1 ]=> (define k)
;Value: k

1 ]=> (+ (call-with-current-continuation
          (lambda (return)
            (set! k return)
            (return (* 3 4))))
         5)
;Value: 17

1 ]=> (k 4)
;Value: 9

1 ]=> (call-with-current-continuation
       (lambda (return)
         (for-each (lambda (thing)
                     (if (< thing 0)
		         (return thing)))
                   '(15 42 8 -3 86 -5))))
;Value: -3

1 ]=> 
