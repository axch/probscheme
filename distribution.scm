;;; ----------------------------------------------------------------------
;;; Copyright 2007 Alexey Radul, Taylor Campbell, and Yu-hsin Chen.
;;; ----------------------------------------------------------------------
;;; This file is part of Probabilistic Scheme.
;;; 
;;; Probabilistic Scheme is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;; 
;;; Probabilistic Scheme is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with Probabilistic Scheme.  If not, see <http://www.gnu.org/licenses/>.
;;; ----------------------------------------------------------------------

;;;; Probability Distributions

(declare (usual-integrations))

(define-structure (distribution
                   (constructor %%make-distribution
                                (stream hash-table determined-density))
                   (conc-name distribution/))
  stream
  (hash-table #f read-only #t)
  determined-density
  (discarded-density 0)
  )

(define (%make-distribution stream hash-table determined-density)
  (letrec ((distribution
            (%%make-distribution
             (lazy
              (let recur ((stream stream))
                (if (stream-pair? stream)
                    (let ((element (stream-car stream))
                          (remainder (lazy (recur (stream-cdr stream)))))
                      ;; This craziness ensures that the stream is
                      ;; kept synchronized with the hash table at all
                      ;; times.
                      (record-element element distribution)
                      (set-distribution/stream! distribution remainder)
                      (stream-cons element remainder))
                    stream-nil)))
             hash-table
             determined-density)))
    distribution))

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
  (set-distribution/determined-density!
   distribution
   (+ (possibility/density possibility)
      (distribution/determined-density distribution))))

(define (record-impossibility impossibility distribution)
  (set-distribution/discarded-density!
   distribution
   (+ (impossibility/density impossibility)
      (distribution/discarded-density distribution))))

(define (distribution/undetermined-density distribution)
  (- 1
     (+ (distribution/determined-density distribution)
        (distribution/discarded-density distribution))))

(define (distribution/datum-density distribution datum)
  (hash-table/get (distribution/hash-table distribution) datum 0))

(define (stream->distribution stream #!optional datum=? datum-hash)
  (%make-distribution stream (make-hash-table datum=? datum-hash) 0))

(define (hash-table->distribution hash-table)
  (%make-distribution stream-nil
                      hash-table
                      (let ((determined-density 0))
                        (hash-table/for-each hash-table
                          (lambda (datum density)
                            datum       ;ignore
                            (set! determined-density
                                  (+ determined-density density))
                            unspecific))
                        determined-density)))

(define (alist->distribution alist #!optional datum=? datum-hash)
  (hash-table->distribution (alist->hash-table alist datum=? datum-hash)))

(define (distribution->density-stream distribution)
  (lazy
   ((lambda (stream)
      (let ((discarded-density (distribution/discarded-density distribution)))
        (if (zero? discarded-density)
            stream
            (stream-cons (make-impossibility discarded-density) stream))))
    (stream-append
     (stream-map (lambda (pair)
                   (make-possibility (car pair) (cdr pair)))
                 (list->stream
                  (hash-table->alist
                   (distribution/hash-table distribution))))
     (distribution/stream distribution)))))

(define (distribution->mass-stream distribution)
  (let ((density-stream (distribution->density-stream distribution))
        (hash-table (distribution/hash-table distribution)))
    (density-stream->mass-stream density-stream
                                 ;; This is the initial discarded density.  We
                                 ;; pass zero because the density stream will
                                 ;; already contain an impossibility as the
                                 ;; first element if there is any discarded
                                 ;; density to begin with.
                                 0
                                 (hash-table-equivalence-function hash-table)
                                 (hash-table-hash-function hash-table))))

(define (distribution->current-density-alist distribution)
  (hash-table->alist (distribution->current-density-hash-table distribution)))

(define (distribution->current-density-hash-table distribution)
  (distribution/hash-table distribution))

(define (distribution->current-bounds-alist distribution)
  (map (lambda (datum-density)
	 (list (car datum-density)
	       (distribution/density->min-mass 
		distribution (cdr datum-density))
	       (distribution/density->max-mass 
		distribution (cdr datum-density))))
       (distribution->current-density-alist distribution)))

;;;; Normalization

(define (distribution/normalized? distribution)
  (and (distribution/determined? distribution)
       (zero? (distribution/discarded-density distribution))))

(define (distribution/min-normalizer distribution)
  (distribution/determined-density distribution))

(define (distribution/max-normalizer distribution)
  (- 1 (distribution/discarded-density distribution)))

(define (distribution/normalizer distribution)
  (if (not (distribution/determined? distribution))
      (error "Incompletely determined distribution has no unique normalizer:"
             distribution))
  (distribution/max-normalizer distribution))

(define (distribution/undetermined-mass distribution)
  (distribution/density->max-mass distribution 0))

(define (distribution/datum-probability distribution datum)
  (distribution/datum-mass distribution datum))

(define (distribution/datum-min-probability distribution datum)
  (distribution/datum-min-mass distribution datum))

(define (distribution/datum-max-probability distribution datum)
  (distribution/datum-max-mass distribution datum))

(define (distribution/datum-mass distribution datum)
  (if (not (distribution/determined? distribution))
      (error "Datum does not have an exact mass in undetermined distribution:"
             datum
             distribution))
  (distribution/datum-min-mass distribution datum))

(define (distribution/datum-min-mass distribution datum)
  (distribution/density->min-mass
   distribution
   (distribution/datum-density distribution datum)))

(define (distribution/datum-max-mass distribution datum)
  (distribution/density->max-mass
   distribution
   (distribution/datum-density distribution datum)))

(define (distribution/density->min-mass distribution density)
  (/ density (distribution/max-normalizer distribution)))

(define (distribution/density->max-mass distribution density)
  (/ (+ density (distribution/undetermined-density distribution))
     (distribution/max-normalizer distribution)))

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

;;;; Incremental Refinement

;;; Variations on compute-until! with specific predicates may be
;;; optimizable for compound distributions by backpropagating the
;;; constraints imposed by the predicate, and thereby allowing the
;;; internal distributions to accumulate density packets for
;;; repeatedly discovered data.

(define (distribution/determined? distribution)
  (stream-null? (distribution/stream distribution)))

(define (distribution/density-bounded? distribution density-bound)
  (<= (distribution/undetermined-density distribution)
      density-bound))

(define (distribution/mass-bounded? distribution mass-bound)
  (<= (distribution/undetermined-mass distribution)
      mass-bound))

(define (distribution/determine! distribution)
  (distribution/refine-until! distribution distribution/determined?))

(define (distribution/refine-to-density-bound! distribution bound)
  (distribution/refine-until! distribution
    (lambda (distribution)
      (distribution/density-bounded? distribution bound))))

(define (distribution/refine-to-mass-bound! distribution bound)
  (distribution/refine-until! distribution
    (lambda (distribution)
      (distribution/mass-bounded? distribution bound))))

(define (distribution/refine-until! distribution predicate)
  (let loop ()
    (cond ((predicate distribution) #t)
          ((distribution/refine! distribution) (loop))
          (else #f))))

;;; Higgledy-Piggledy,
;;; Distribution refine-
;;; Ment is a really quite
;;; Horrible blow.             (`Hack' doesn't rhyme too well here.)
;;; %MAKE-DISTRIBUTION's con-
;;; Struction of streams would de-
;;; Structively alter the
;;; Stream as we go.

;;; This returns whether or not we actually refined the distribution
;;; at all.

(define (distribution/refine! distribution)
  (pair? (force (distribution/stream distribution))))

;;;; Distribution Combinators

(define (make-discrete-distribution . options)
  (alist->distribution
   (map (lambda (option)
          (cons (car option) (cadr option)))
        options)))

(define (map-distribution distribution datum-mapper)
  (stream->distribution
   (lazy
    (stream-map-possibilities
     (let ((hash-table
            (let ((hash-table (distribution/hash-table distribution)))
              (make-hash-table (hash-table-equivalence-function hash-table)
                               (hash-table-hash-function hash-table)))))
       (lambda (possibility)
         (make-possibility (let ((datum (possibility/datum possibility)))
                             (hash-table/intern! hash-table datum
                               (lambda ()
                                 (datum-mapper datum))))
                           (possibility/density possibility))))
     (distribution->density-stream distribution)))))

(define (conditional-distribution distribution predicate)
  (stream->distribution
   (stream-map-possibilities
    (lambda (possibility)
      (if (predicate (possibility/datum possibility))
          possibility
          (make-impossibility (possibility/density possibility))))
    (distribution->density-stream distribution))))

(define (stream-map-possibilities procedure stream)
  (stream-map (lambda (element)
                (if (possibility? element)
                    (procedure element)
                    element))
              stream))

(define (distribution-bind distribution function)
  (dependent-product distribution function (lambda (x y) y)))

(define (dependent-product distribution generator combiner
                           #!optional datum=? datum-hash)
  ((lambda (stream) (stream->distribution stream datum=? datum-hash))
   (stream-diagonalize
    (stream-map-possibilities
     (lambda (possibility)
       (stream-map (lambda (element)
                     (dependent-product-combine possibility element combiner))
                   (distribution->density-stream
                    (generator (possibility/datum possibility)))))
     (distribution->density-stream distribution)))))

(define (dependent-product-combine possibility element combiner)
  (cond ((possibility? element)
         (make-possibility (combiner (possibility/datum possibility)
                                     (possibility/datum element))
                           (* (possibility/density possibility)
                              (possibility/density element))))
        ((impossibility? element)
         (make-impossibility
          (* (possibility/density possibility)
             (impossibility/density element))))
        (else
         (error "Invalid element in distribution stream:" element))))

(define (independent-product distribution-a distribution-b combiner)
  (dependent-product distribution-a
                     (lambda (datum)
                       datum            ;ignore
                       distribution-b)
                     combiner))

(define (mixture . options)
  (let ((meta-distribution (apply make-discrete-distribution options)))
    (dependent-product
     meta-distribution
     (lambda (distribution) distribution)
     (lambda (distribution datum)
       distribution   ;ignore
       datum))))

(define (bayes-rule prior-distribution likelihood-function)
  (let ((product
	 (dependent-product
	  prior-distribution
	  (lambda (datum)
	    (bernoulli-distribution (likelihood-function datum)))
	  cons)))
    (map-distribution (conditional-distribution product cdr) car)))

(define (sample distribution)
  (let loop ((target (random 1.0))
	     (stream (distribution->mass-stream distribution)))
    (if (<= target (possibility/density (stream-car stream)))
	(possibility/datum (stream-car stream))
	(loop (- target (possibility/density (stream-car stream)))
	      (stream-cdr stream)))))
