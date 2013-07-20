;;; ----------------------------------------------------------------------
;;; Copyright 2007 Alexey Radul.
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

(define-structure
  (single-test (constructor make-single-test (name thunk))
	       (conc-name st:))
  (name 'nameless read-only #t)
  (thunk #f read-only #t))

(define-structure 
  (test-group (constructor make-test-group (name))
	      (conc-name tg:))
  (name 'nameless read-only #t)
  (group-set-up (lambda () 'done))
  (group-tear-down (lambda () 'done))
  (set-up (lambda () 'done))
  (tear-down (lambda () 'done))
  (test-map (make-ordered-map) read-only #t))

(define (tg:register-test! group test)
  (omap:put! (tg:test-map group) (st:name test) test))

(define (tg:find-or-make-subgroup group name)
  (let ((subgroup (omap:get (tg:test-map group) name #f)))
    (cond ((not subgroup)
	   (tg:make-subgroup! group name))
	  ((procedure? subgroup)
	   (error "Namespace collision between tests and subgroups" group name))
	  (else subgroup))))

(define (tg:make-subgroup! group name)
  (let ((new-group (make-test-group name)))
    (omap:put! (tg:test-map group) name new-group)
    new-group))

(define (tg:get group name)
  (cond ((null? name) group)
	((pair? name)
	 (tg:get (tg:get group (car name)) (cdr name)))
	(else
	 (omap:get (tg:test-map group) name #f))))

(define *current-test-group* (make-test-group 'top-level))

(define (current-test-group) *current-test-group*)

(define (with-top-level-group group thunk)
  (fluid-let ((*current-test-group* group))
    (thunk)))

(define-syntax in-test-group
  (syntax-rules ()
    ((_ name body-exp ...)
     (let ((group (tg:find-or-make-subgroup *current-test-group* 'name)))
       (fluid-let ((*current-test-group* group))
	 body-exp ...)
       group))))

(define (*define-group-set-up thunk)
  (set-tg:group-set-up! (current-test-group) thunk))

(define (*define-group-tear-down thunk)
  (set-tg:group-tear-down! (current-test-group) thunk))

(define (*define-set-up thunk)
  (set-tg:set-up! (current-test-group) thunk))

(define (*define-tear-down thunk)
  (set-tg:tear-down! (current-test-group) thunk))

(define-syntax define-group-set-up
  (syntax-rules ()
    ((_ body-exp ...)
     (*define-group-set-up
      (lambda ()
	body-exp ...)))))

(define-syntax define-group-tear-down
  (syntax-rules ()
    ((_ body-exp ...)
     (*define-group-tear-down
      (lambda ()
	body-exp ...)))))

(define-syntax define-set-up
  (syntax-rules ()
    ((_ body-exp ...)
     (*define-set-up
      (lambda ()
	body-exp ...)))))

(define-syntax define-tear-down
  (syntax-rules ()
    ((_ body-exp ...)
     (*define-tear-down
      (lambda ()
	body-exp ...)))))
