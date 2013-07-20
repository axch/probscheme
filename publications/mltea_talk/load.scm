(define (load-relative filename)
  (with-working-directory-pathname 
   (directory-namestring (current-load-pathname))
   (lambda () (load filename))))

(load-relative "boring-magic-1")
(load-relative "boring-magic-2")
(load-relative "boring-mundanity-1")
(load-relative "select-observe")
(load-relative "with-probabilistic-choices")
(load-relative "choice-try")
(load-relative "tree")
(load-relative "mutation-process")
(load-relative "model")
