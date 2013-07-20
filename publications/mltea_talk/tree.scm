(define *real-kemp-tenenbaum-tree*
  '(0 (0.13 (0.1337 (0.3863 (0.03 chimp) (0.03 gorilla))
                    (0.1963 (0.15 (0.07 horse) (0.07 cow))
			    (0.14 (0.08 elephant)
				  (0.08 rhino))))
	    (0.49 (0.06 mouse) (0.06 squirrel)))
      (0.60 (0.08 dolphin) (0.08 seal))))

;; The talk featured the real tree, but it fails tests
(define *kemp-tenenbaum-tree*
  '(0 (1/4 (1/4 (1/2 (0 chimp) (0 gorilla))
		(1/6 (1/6 (1/6 horse) (1/6 cow))
		     (1/6 (1/6 elephant) (1/6 rhino))))
	   (3/4 (0 mouse) (0 squirrel)))
      (5/6 (1/6 dolphin) (1/6 seal))))

(define (tree-lead-length tree) (car tree))

(define (tree-terminal? tree) (symbol? (cadr tree)))

(define (tree-terminal tree) (cadr tree))

(define (tree-left-branch tree) (cadr tree))

(define (tree-right-branch tree) (caddr tree))
