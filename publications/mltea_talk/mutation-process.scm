(define (generate-concept mutation-rate tree)
  (define (walk concept-on tree)
    (if (or concept-on 
	    (turn-concept-on? mutation-rate
			      (tree-lead-length tree)))
	(if (tree-terminal? tree)
	    (list (tree-terminal tree))
	    (append (walk #t (tree-left-branch tree))
		    (walk #t (tree-right-branch tree))))
	(if (tree-terminal? tree)
	    '()
	    (append (walk #f (tree-left-branch tree))
		    (walk #f (tree-right-branch tree))))))
  (walk #f tree))

(define (turn-concept-on? mutation-rate branch-length)
  (boolean-select (- 1 (exp (* -1 mutation-rate
			       branch-length)))))

(define (boolean-select prob-true)
  (discrete-select `(#t . ,prob-true)
		   `(#f . ,(- 1 prob-true))))

