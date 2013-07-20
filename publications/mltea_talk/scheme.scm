1 ]=> (+ (* 3 4) 5)
;Value: 17

1 ]=> (define x 5)
;Value: x

1 ]=> x
;Value: 5

1 ]=> (define (add2 x)
        (+ x 2))
;Value: add2

1 ]=> (add2 2)
;Value: 4

1 ]=> (let ((x 7)
            (add3 (lambda (x) (+ x 3))))
        (add3 x))
;Value: 10

1 ]=> (car '(1 2))
;Value: 1

1 ]=> (cdr '(1 2))
;Value 11: (2)

1 ]=> (cadr '(1 2))
;Value: 2

1 ]=> (cadr `(1 ,x))
;Value: 5

1 ]=> 
