;;; A distribution is really just a cache in front of a stream of
;;; possibilities (and impossibilities)

(define-structure (distribution
                   (constructor %%make-distribution
                                (stream hash-table determined-density))
                   (conc-name distribution/))
  stream (hash-table #f read-only #t)
  determined-density (discarded-density 0))

(define (%make-distribution stream hash-table determined-density)
  (letrec ((distribution
            (%%make-distribution
             (lazy
              (let recur ((stream stream))
                (if (stream-pair? stream)
                    (let ((element (stream-car stream))
                          (remainder (lazy (recur (stream-cdr stream)))))
                      ;; This craziness ensures that the stream is kept
                      ;; synchronized with the hash table at all times.
                      (record-element element distribution)
                      (set-distribution/stream! distribution remainder)
                      (stream-cons element remainder))
                    stream-nil)))
             hash-table
             determined-density)))
    distribution))

(define (stream->distribution stream #!optional datum=? datum-hash)
  (%make-distribution stream (make-hash-table datum=? datum-hash) 0))

;;; Recording elements

(define (record-element element distribution)
  (cond ((possibility? element)
         (record-possibility element distribution))
        ((impossibility? element)
         (record-impossibility element distribution))
        (else
         (error "Invalid element in distribution stream:" element))))

(define (record-possibility possibility distribution)
  (hash-table/modify! (distribution/hash-table distribution)
    (possibility/datum possibility)
    0
    (lambda (density)
      (+ density (possibility/density possibility))))
  (set-distribution/determined-density! distribution
    (+ (possibility/density possibility)
       (distribution/determined-density distribution))))

(define (record-impossibility impossibility distribution)
  (set-distribution/discarded-density! distribution
    (+ (impossibility/density impossibility)
       (distribution/discarded-density distribution))))

;;; Querying for probabilities of things

(define (distribution/datum-density distribution datum)
  (hash-table/get (distribution/hash-table distribution) datum 0))

(define (distribution/datum-min-mass distribution datum)
  (distribution/density->min-mass distribution
    (distribution/datum-density distribution datum)))

(define (distribution/datum-max-mass distribution datum)
  (distribution/density->max-mass distribution
    (distribution/datum-density distribution datum)))

(define (distribution/density->min-mass distribution density)
  (/ density (distribution/max-normalizer distribution)))

(define (distribution/density->max-mass distribution density)
  (/ (+ density (distribution/undetermined-density distribution))
     (distribution/max-normalizer distribution)))

(define (distribution/max-normalizer distribution)
  (- 1 (distribution/discarded-density distribution)))

(define (distribution/undetermined-density distribution)
  (- 1 (+ (distribution/determined-density distribution)
          (distribution/discarded-density distribution))))

;;; Higgledy-Piggledy,
;;; Distribution refine-
;;; Ment is a really quite
;;; Horrible blow.             (`Hack' doesn't rhyme too well here.)
;;; %MAKE-DISTRIBUTION's con-
;;; Struction of streams would de-
;;; Structively alter the
;;; Stream as we go.

(define (distribution/refine! distribution)
  ;; This pair? test checks whether we actually refined the distribution
  (pair? (force (distribution/stream distribution))))

(define (distribution->alist distribution)
  (hash-table->alist
   (distribution/hash-table (normalize-determine! distribution))))

(define (normalize-determine! distribution)
  (distribution/determine! distribution)
  (let ((result (distribution/normalize distribution)))
    (distribution/determine! result)
    result))

(define (distribution/determine! distribution)
  (distribution/refine-until! distribution distribution/determined?))

(define (distribution/refine-until! distribution predicate)
  (let loop ()
    (cond ((predicate distribution) #t)
          ((distribution/refine! distribution) (loop))
          (else #f))))

(define (distribution/determined? distribution)
  (stream-null? (distribution/stream distribution)))

(define (distribution/normalize distribution)
  (if (not (distribution/determined? distribution))
      (error "Can't normalize an undetermined distribution:" distribution))
  (stream->distribution
   (stream-map (let ((normalizer (distribution/max-normalizer distribution)))
                 (lambda (possibility)
                   (make-possibility (possibility/datum possibility)
                                     (/ (possibility/density possibility)
                                        normalizer))))
               (stream-filter possibility?
                              (distribution->density-stream distribution)))))
