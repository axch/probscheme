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

;; Even though that's a little inconvenient, the urns are functional
;; because there is otherwise no way to backtrack changes to them.  In
;; particular, it seems that define-structure would need to be
;; upgraded to produce amb-set-urn-red! functions.
(define-structure
  (urn (constructor make-urn (red blue)))
  (red 0)
  (blue 0))

(define (remove-ball urn color)
  (case color
    ((red) (make-urn (- (urn-red urn) 1) (urn-blue urn)))
    ((blue) (make-urn (urn-red urn) (- (urn-blue urn) 1)))))

(define (add-ball urn color)
  (case color
    ((red) (make-urn (+ (urn-red urn) 1) (urn-blue urn)))
    ((blue) (make-urn (urn-red urn) (+ (urn-blue urn) 1)))))

(define (urn-size urn)
  (+ (urn-red urn) (urn-blue urn)))

(define (draw-random-ball urn)
  (let ((color (discrete-select ('red  (/ (urn-red urn)  (urn-size urn)))
				('blue (/ (urn-blue urn) (urn-size urn))))))
    (values color (remove-ball urn color))))

(define (joyce-draw urn)  
  (let ((color (discrete-select ('red  (/ (urn-red urn)  (urn-size urn)))
				('blue (/ (urn-blue urn) (urn-size urn))))))
    (values color (if (eq? color 'red)
		      urn ; put red balls back
		      (remove-ball urn color)))))

(define (draw-sequence drawer urn trials)
  (let loop ((urn urn)
	     (drawn-so-far '())
	     (trials-left trials))
    (if (= 0 trials-left)
	(reverse drawn-so-far)
	(call-with-values (lambda () (drawer urn))
	  (lambda (color new-urn)
	    (loop new-urn
		  (cons color drawn-so-far)
		  (- trials-left 1)))))))
