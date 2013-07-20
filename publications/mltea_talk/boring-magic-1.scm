(define-structure
  (selection-cell (constructor
		   make-selection-cell
		   (continuation option-list prior-mass
				 likelihood))
		  (conc-name cell/))
  (continuation #f read-only #t)
  (option-list '() read-only #t)
  (prior-mass 0 read-only #t)
  (likelihood 0 read-only #t))

(define (cell-cdr-options cell)
  (make-selection-cell (cell/continuation cell)
		       (cdr (cell/option-list cell))
		       (cell/prior-mass cell)
		       (cell/likelihood cell)))

(define (next-option-value option-list)
  (caar option-list))

(define (next-option-prior option-list)
  (cdar option-list))

