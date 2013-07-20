(define die-roll-distribution
  (make-discrete-distribution '(1 1/6) '(2 1/6) '(3 1/6)
			      '(4 1/6) '(5 1/6) '(6 1/6)))

(let* ((two-roll-distribution
	(indepedent-product
	 die-roll-distribution die-roll-distribution +))
       (two-even-roll-distribution
	(conditional-distribution two-roll-distribution even?)))
  two-roll-distribution)

(define (roll-die)
  (discrete-select (1 1/6) (2 1/6) (3 1/6)
		   (4 1/6) (5 1/6) (6 1/6)))

(stochastic-thunk->distribution
 (lambda ()
   (let ((num (+ (roll-die)
		 (roll-die)
		 )))
     (observe! (even? num))
     num)))

(stochastic-thunk->distribution
 (lambda ()
   (let ((face1 (roll-die)))
     (if (even? face1)
	 (+ face1 (roll-die))
	 3))))

(stochastic-thunk->distribution
 (lambda ()
   (let ((length (roll-die)))
     (apply * (map (lambda (ignore)
		     (roll-die))
		   (make-list 1 length))))))

(dependent-product
 die-roll-distribution
 (lambda (face1)
   (if (even? face1)
       (indepedent-product (make-discrete-distribution (list face1 1))
			   die-roll-distribution
			   +)
       (make-discrete-distribution (list 3 1))))
 (lambda (face1 result) result))

(define (geometric-select start alpha)
  (discrete-select (start alpha)
		   ((geometric-select (+ start 1) alpha) (- 1 alpha))))

(define (geometric-distribution start alpha)
  (make-discrete-distribution
   (list start alpha)
   (splice (geometric-distribution (+ 1 start) alpha)
	   (- 1 alpha))))

(define (geometric-distribution start alpha)
  (dependent-product
   (make-discrete-distribution
    '(stop alpha)
    `(go ,(- 1 alpha)))
   (lambda (symbol)
     (if (eq? symbol 'stop)
	 (make-discrete-distribution (list start 1))
	 (geometric-distribution (+ start 1) alpha)))
   (lambda (ignore result) result)))
