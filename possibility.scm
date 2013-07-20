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

;;;; Possibilities and Impossibilities

(declare (usual-integrations))

;;;; Possibilities and Impossibilities

;;; This page is too short.  I am sorry for wasting your paper.  You
;;; are welcome to grace this page's presence with doodles below the
;;; micro-oodle of code here.

(define-structure (possibility
                   (constructor make-possibility (datum density))
                   (conc-name possibility/)
                   (print-procedure
                    (simple-unparser-method 'POSSIBILITY
                      (lambda (possibility)
                        (list (possibility/datum possibility)
                              (possibility/density possibility))))))
  (datum #f read-only #t)
  (density #f read-only #t)
  )

(define-structure (impossibility
                   (constructor make-impossibility (density))
                   (conc-name impossibility/)
                   (print-procedure
                    (simple-unparser-method 'IMPOSSIBILITY
                      (lambda (impossibility)
                        (list (impossibility/density impossibility))))))
  (density #f read-only #t)
  )

;;;; Density Streams and Mass Streams

;;; This incrementally normalizes each element of the density stream,
;;; and if any impossibility arises, redistributes new mass to each
;;; datum already considered by emitting a new possibility for each
;;; such datum whose density is the difference between the newly
;;; computed mass and the old mass we had computed before the
;;; impossibility.  Throughout the process, we record the density and
;;; the current mass for each datum, in order to compute the new mass
;;; and the mass difference after considering an impossibility.
;;;
;;; For example, if we see the stream [(A 1/4) (B 1/4) 1/4 (C 1/4)],
;;; where (A 1/4) is a possibility and a bare 1/4 is an impossibility,
;;; then we first emit [(A 1/4) (B 1/4)] in the mass stream; after
;;; that, we see that the mass of A and B is really 1/3, now that 1/4
;;; of the density has been concluded impossible, and we emit [(A 1/6)
;;; (B 1/6)] to compensate; finally, we record that C's density is
;;; 1/4, and we normalize that to a mass of 1/3, yielding a full
;;; stream of [(A 1/4) (B 1/4) (A 1/6) (B 1/6) (C 1/3)].

(define (density-stream->mass-stream density-stream discarded-density
                                     #!optional datum=? datum-hash)
  (lazy
   (let ((cache (make-hash-table datum=? datum-hash)))
     (let recur ((density-stream density-stream)
                 (discarded-density discarded-density))
       (if (not (stream-pair? density-stream))
           stream-nil
           (let ((element (stream-car density-stream))
                 (density-stream (stream-cdr density-stream)))
             (cond ((possibility? element)
                    (stream-cons
                     (mass-possibility element cache discarded-density)
                     (lazy (recur density-stream discarded-density))))
                   ((impossibility? element)
                    (let ((discarded-density
                           (+ discarded-density
                              (impossibility/density element))))
                      (stream-append
                       (mass-impossibility element cache discarded-density)
                       (lazy (recur density-stream discarded-density)))))
                   (else
                    (error "Invalid element in density stream:"
                           element)))))))))

(define (mass-possibility possibility cache discarded-density)
  (let ((datum (possibility/datum possibility))
        (density (possibility/density possibility)))
    (let ((mass (/ density (- 1 discarded-density))))
      (hash-table/modify! cache datum
        (cons 0 0)
        (lambda (density.mass)
          (cons (+ density (car density.mass)) (+ mass (cdr density.mass)))))
      (make-possibility datum mass))))

(define (mass-impossibility impossibility cache discarded-density)
  (list->stream
   (map! (lambda (entry)
           (let ((datum (car entry))
                 (density (cadr entry))
                 (mass (cddr entry)))
             (let ((mass* (/ density (- 1 discarded-density))))
               (hash-table/put! cache datum (cons density mass*))
               (make-possibility datum (- mass* mass)))))
         (hash-table->alist cache))))
