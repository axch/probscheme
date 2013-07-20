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
  (ordered-map (constructor make-ordered-map ()) (conc-name omap:))
  (entry-list #f)
  (entry-table (make-equal-hash-table)))

(define-structure (omap-entry)
  (key #f)
  (item #f)
  (next #f)
  (prev #f))

(define (omap:fetch-entry omap key)
  (hash-table/get (omap:entry-table omap) key #f))

(define (omap:put! omap key datum)
  (let ((entry (omap:fetch-entry omap key)))
    (if entry 
	(set-omap-entry-item! entry datum)
	(omap:put-new-entry! omap key datum))))

(define (omap:put-new-entry! omap key datum)
  (let* ((head (omap:entry-list omap))
	 (new-entry (make-omap-entry key datum head #f)))
    (if head (set-omap-entry-prev! head new-entry))
    (set-omap:entry-list! omap new-entry)
    (hash-table/put! (omap:entry-table omap) key new-entry)))

(define (omap:get omap key default)
  (let ((entry (omap:fetch-entry omap key)))
    (if entry 
	(omap-entry-item entry)
	default)))

(define (omap:remove! omap key)
  (let ((entry (omap:fetch-entry omap key)))
    (if entry
	(omap:remove-entry! omap key entry))))

(define (omap:remove-entry! omap key entry)
  (hash-table/remove! (omap:entry-table omap) key)
  (let ((old-prev (omap-entry-prev entry))
	(old-next (omap-entry-next entry)))
    (if old-prev (set-omap-entry-next! old-prev old-next))
    (if old-next (set-omap-entry-prev! old-next old-prev))))

(define (omap:count omap)
  (hash-table/count (omap:entry-table omap)))

(define (omap:key-list omap)
  (reverse
   (let loop ((head (omap:entry-list omap)))
     (if head
	 (cons (omap-entry-key head)
	       (loop (omap-entry-next head)))
	 '()))))

(define (omap:for-each omap procedure)
  (let loop ((head (omap:entry-list omap)))
    (if head
	(begin (loop (omap-entry-next head))
	       (procedure (omap-entry-key head) (omap-entry-item head)))
	unspecific)))

