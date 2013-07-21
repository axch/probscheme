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

(declare (usual-integrations))

;;;; Hangman Player

;;; Batch mode:
;;; 1. Load probscheme
;;; 2. Load this file
;;; 3. Evaluate (prepare-hangman <length>) to prepare the program to guess
;;;    words of the given length
;;; 4. Evaluate (play-hangman <true-word> hangman-guess-letter) to
;;;    watch the program guess the given word.  The word ought to be
;;;    the length given.
;;;    - The outputs indicate the current score, the program's current
;;;      revealed letters, and the letters it has guessed so far.
;;;    - The return value is the number of tries it needed to guess the word.

;;;; Utilities

(load-relative-compiled "../decisions")

(define *alphabet*
  (string->list "qwertyuiopasdfghjklzxcvbnm"))

(define (slurp-corpus filename len #!optional limit dictionary-only)
  (if (default-object? limit) (set! limit 10000))
  (if (default-object? dictionary-only) (set! dictionary-only #f))
  (with-input-from-file 
      filename
    (lambda ()
      (let loop ((result '())
		 (limit limit))
	(let* ((word (read))
	       (count (read)))
	  (if dictionary-only (set! count 1))
	  (if (or (= limit 0) (eof-object? word))
	      (reverse result)
	      (if (= len (string-length word))
		  (loop (cons (cons word count) result) (- limit 1))
		  (loop result limit))))))))

(define (alist-average alist)
  (/ (reduce + 0 (map cdr alist))
     (length alist)))

;;;; States of a game of Hangman

(define-structure
  (state
   (constructor %make-state)
   (print-procedure
    (simple-unparser-method
     'state
     (lambda (state)
       (list (state-score state)
	     (signature->string (state-signature state))
	     (list->string (reverse (state-letters-tried state))))))))
  signature
  letters-tried
  score)

(define *state-wildcard* #\.)

(define (signature->string signature)
  (list->string
   (map (lambda (signature-piece)
	  (if (eq? signature-piece 'any-char)
	      *state-wildcard*
	      signature-piece))
	signature)))

(define (string->signature string)
  (map (lambda (character)
	 (if (equal? *state-wildcard* character)
	     'any-char
	     character))
       (string->list string)))

(define (state-matches? state word)
  (let loop ((index 0)
	     (signature-left (state-signature state)))
    (if (null? signature-left)
	#t
	(let ((signature-component (car signature-left))
	      (word-character (string-ref word index)))
	  (cond ((char? signature-component)
		 (and (equal? signature-component word-character)
		      (loop (+ index 1) (cdr signature-left))))
		(else
		 (and (not (member word-character (state-letters-tried state)))
		      (loop (+ index 1) (cdr signature-left)))))))))

(define (state-initial? state)
  (for-all? (state-signature state)
	    (lambda (signature-element)
	      (eq? signature-element 'any-char))))

(define (state-finished? state)
  (not (member 'any-char (state-signature state))))

(define (state-num-letters state)
  (length (state-signature state)))

(define (state-possible-letters state)
  (delete-matching-items
   *alphabet*
   (lambda (letter)
     (member letter (state-letters-tried state)))))

(define (state-equal? state1 state2)
  (and (equal? (state-signature state1) (state-signature state2))
       (equal? (sort (state-letters-tried state1) char<?)
	       (sort (state-letters-tried state2) char<?))))

(define (true-word->state true-word letters)
  (let* ((signature 
	  (map (lambda (word-char)
		 (if (member word-char letters)
		     word-char
		     'any-char))
	       (string->list true-word)))
	 (score (length (filter (lambda (guessed-letter)
				  (not (member guessed-letter signature)))
				letters))))
    (%make-state signature letters score)))

(define (update-state true-word state new-letter)
  (true-word->state true-word (cons new-letter (state-letters-tried state))))

(define (initial-state true-word)
  (true-word->state true-word '()))


;;;; Hangman Game Engine

(define (word-count-alist->hangman-prior alist)
  (let ((sum (reduce + 0 (map cdr alist))))
    (alist->distribution
     (map (lambda (word-count)
	    (cons (car word-count) (/ (cdr word-count) sum)))
	  alist))))

(define *hangman-prior*)

;; I imagine enough players will be deterministic that this is worth
;; providing for all of them.
(define *hangman-opening-cache*)

(define (prepare-hangman word-length #!optional limit dictionary-only)
  (set! *hangman-opening-cache* '())
  (set! *hangman-prior* #f) ; Clear space while building the next one
  (set! *hangman-prior*
	(word-count-alist->hangman-prior
	 (slurp-corpus "data/google-words" word-length limit dictionary-only))))

(define (hangman-cache-lookup state)
  (let loop ((cache *hangman-opening-cache*))
    (cond ((null? cache) #f)
	  ((state-equal? state (caar cache)) (cdar cache))
	  (else
	   (loop (cdr cache))))))

(define (hangman-cache-save! state move)
  (set! *hangman-opening-cache*
	(cons (cons state move)
	      *hangman-opening-cache*)))

(define (play-hangman true-word player)
  (display "Trying to guess ")
  (display true-word)
  (newline)
  (let game-loop ((state (initial-state true-word)))
    (display state)
    (newline)
    (if (state-finished? state)
	(state-score state)
	(if (>= (state-score state) 8)
	    'player-lost
	    (let ((letter (player state)))
	      (if letter
		  (game-loop (update-state true-word state letter))
		  'player-gave-up))))))

(define (hangman-random-word)
  (sample *hangman-prior*))

(define (hangman-average-score num-games player)
  (let loop ((runs-left num-games)
	     (total-score 0)
	     (results '()))
    (if (zero? runs-left)
	(begin
	  (pp (reverse results))
	  (/ total-score num-games))
	(let* ((word (hangman-random-word))
	       (result (play-hangman word player)))
	  (if (number? result)
	      ; Player won in that many steps
	      (loop (- runs-left 1)
		    (+ total-score result)
		    (cons (cons word result) results))
	      ; Player lost
	      (loop (- runs-left 1)
		    (+ total-score 1000)
		    (cons (cons word result) results)))))))

(define (hangman-multirun-average player #!optional dictionary-only)
  (let ((results
	 (map (lambda (num-letters)
		(prepare-hangman num-letters 10000 dictionary-only)
		(cons num-letters
		      (hangman-average-score 20 player)))
	      '(5 6 7 8 9 10 11 12 13 14 15))))
    (pp results)
    (alist-average results)))

;;;; Probabilistic Hangman Player

(define (hangman-belief state prior)
  (conditional-distribution
   prior
   (lambda (word)
     (state-matches? state word))))

(define (hangman-loss-function letter-guessed true-word)
  (if (member letter-guessed (string->list true-word))
      0
      1))

(define *hangman-current-belief*)

(define (hangman-choose-letter state)
  (decide (state-possible-letters state)
	  *hangman-current-belief*
	  hangman-loss-function
	  0 1 1e-10))

(define (hangman-do-opening state)
  (set! *hangman-current-belief* *hangman-prior*)
  (let ((opening (hangman-cache-lookup state)))
    (if opening
	opening
	(begin
	  (display "Considering opening... ")
	  (let ((move (hangman-choose-letter state)))
	    (hangman-cache-save! state move)
	    move)))))

(define (hangman-do-midgame state)
  (display "Updating beliefs... ")
  (set! *hangman-current-belief*
	(hangman-belief state *hangman-current-belief*))
  (display "deciding... ")
  (hangman-choose-letter state))

(define (hangman-guess-letter state)
  (if (state-initial? state)
      (hangman-do-opening state)
      (hangman-do-midgame state)))  

;;;; Stupid Hangman Player

(define *letters* "eirsntaocdlpugmhvybfwkxqjz")

(define (stupid-hangman-move state)
  (string-ref *letters* (length (state-letters-tried state))))
