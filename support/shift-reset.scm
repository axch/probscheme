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

;;;; Shift & Reset

(declare (usual-integrations))

(define *meta-continuation*
  (lambda (value)
    (error "You forgot the top-level RESET..." value)))

(define (*abort thunk)
  (let ((value (thunk)))
    ;; Do not beta-reduce this, because running the thunk
    ;; may change the *meta-continuation*.
    (*meta-continuation* value)))

(define (*reset thunk)
  (call-with-current-continuation
    (lambda (k)
      (let ((mc *meta-continuation*))
        (set! *meta-continuation*
              (lambda (value)
                (set! *meta-continuation* mc)
                (k value)))
        (*abort thunk)))))

(define (*shift receiver)
  (call-with-current-continuation
    (lambda (k)
      (*abort (lambda ()
                (receiver (lambda (value)
                            (*reset (lambda ()
                                      (k value))))))))))

(define-syntax reset
  (syntax-rules ()
    ((RESET expression)
     (*RESET (LAMBDA () expression)))))

(define-syntax shift
  (syntax-rules ()
    ((SHIFT variable expression)
     (*SHIFT (LAMBDA (variable) expression)))))
