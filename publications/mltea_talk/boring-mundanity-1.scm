(define (member? thing list)
  (and (member thing list) #t))

(define (same-elements? list1 list2)
  (equal? (sort list1 symbol<?) (sort list2 symbol<?)))

(define (make-discrete-distribution . args)
  (normalize (collapse-duplicates args)))

(define (collapse-duplicates list-distrib)
  (let ((object-hash (make-equal-hash-table)))
    (for-each
     (lambda (obj-prob)
       (let* ((object (car obj-prob))
	      (new-prob (cadr obj-prob))
	      (old-prob (hash-table/get
			 object-hash object 0))
	      (total-prob (+ new-prob old-prob)))
	 (hash-table/put! object-hash object total-prob)))
     list-distrib)
    (map (lambda (key)
	   (list key (hash-table/get object-hash key #f)))
	 (hash-table/key-list object-hash))))

(define (normalize list-distrib)
  (let ((total (reduce + 0 (map cadr list-distrib))))
    (map (lambda (obj-prob)
	   (list (car obj-prob)
		 (/ (cadr obj-prob) total)))
	 list-distrib)))

(define (probability-of distribution object)
  (let ((obj-prob (assoc object distribution)))
    (if obj-prob (cadr obj-prob) 0)))
