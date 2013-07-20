(define (concept-containing premises)
  (let ((concept
	 (generate-concept 0.1 *kemp-tenenbaum-tree*)))
    (for-each (lambda (animal)
		(observe! (member? animal concept)))
	      premises)
    concept))

(define (specific-argument-strength premises conclusion)
  (probability-of
   (with-probabilistic-choices
    (lambda ()
      (member? conclusion (concept-containing premises))))
   #t))

(define (general-argument-strength premises)
  (probability-of
   (with-probabilistic-choices
    (lambda ()
      (same-elements?
       (concept-containing premises)
       (list 'chimp 'cow 'dolphin 'elephant 'gorilla
	     'horse 'mouse 'rhino 'seal 'squirrel))))
   #t))
