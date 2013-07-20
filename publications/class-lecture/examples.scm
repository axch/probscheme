(define (roll-die)
  (discrete-select
   (1 1/6) (2 1/6) (3 1/6) (4 1/6) (5 1/6) (6 1/6)))

(distribution->alist
 (stochastic-thunk->distribution roll-die))
;Value 13: ((6 . 1/6) (5 . 1/6) (4 . 1/6) (3 . 1/6) (2 . 1/6) (1 . 1/6))

(pp
 (distribution->alist
  (stochastic-thunk->distribution
   (lambda () (cons (roll-die) (roll-die))))))
(((6 . 6) . 1/36) ((6 . 5) . 1/36) ((5 . 6) . 1/36) ((6 . 4) . 1/36)
 ((4 . 6) . 1/36) ((5 . 5) . 1/36) ((3 . 6) . 1/36) ((6 . 3) . 1/36)
 ((5 . 4) . 1/36) ((4 . 5) . 1/36) ((2 . 6) . 1/36) ((3 . 5) . 1/36)
 ((4 . 4) . 1/36) ((6 . 2) . 1/36) ((5 . 3) . 1/36) ((5 . 2) . 1/36)
 ((4 . 3) . 1/36) ((6 . 1) . 1/36) ((3 . 4) . 1/36) ((2 . 5) . 1/36)
 ((1 . 6) . 1/36) ((3 . 3) . 1/36) ((4 . 2) . 1/36) ((5 . 1) . 1/36)
 ((2 . 4) . 1/36) ((1 . 5) . 1/36) ((2 . 3) . 1/36) ((4 . 1) . 1/36)
 ((3 . 2) . 1/36) ((1 . 4) . 1/36) ((1 . 3) . 1/36) ((3 . 1) . 1/36)
 ((2 . 2) . 1/36) ((1 . 2) . 1/36) ((2 . 1) . 1/36) ((1 . 1) . 1/36))

(pp
 (distribution->alist
  (stochastic-thunk->distribution
   (lambda () (+ (roll-die) (roll-die))))))
((12 . 1/36) (11 . 1/18) (10 . 1/12) (9 . 1/9) (8 . 5/36) (7 . 1/6)
 (6 . 5/36) (5 . 1/9) (4 . 1/12) (3 . 1/18) (2 . 1/36))

(pp
 (distribution->alist
  (stochastic-thunk->distribution
   (lambda ()
     (+ (discrete-select (1 1/6) (2 1/6) (2 1/6) (3 1/6) (3 1/6) (4 1/6))
	(discrete-select (1 1/6) (3 1/6) (4 1/6) (5 1/6) (6 1/6) (8 1/6)))))))
((12 . 1/36) (11 . 1/18) (10 . 1/12) (9 . 1/9) (8 . 5/36) (7 . 1/6)
 (6 . 5/36) (5 . 1/9) (4 . 1/12) (3 . 1/18) (2 . 1/36))

(distribution->alist
 (stochastic-thunk->distribution
  (lambda () 
    (let ((answer (+ (roll-die) (roll-die))))
      (observe! (> answer 9))
      answer))))
;Value 15: ((12 . 1/6) (11 . 1/3) (10 . 1/2))

;;; Suppose I have three cards: One is red on both sides, one is blue
;;; on both sides, and one is red on one side and blue on the other.
;;; I shuffle the cards and put one on the table.  The side facing up
;;; turns out to be red.  What is the probability that the other side
;;; is red, too?

(distribution->alist
 (stochastic-thunk->distribution
  (lambda ()
    (let* ((card (discrete-select ('(red . red) 1/3)
				  ('(blue . blue) 1/3)
				  ('(red . blue) 1/3)))
	   (side-shown ((discrete-select (car 1/2) (cdr 1/2)) card))
	   (other-side (if (eq? side-shown (car card))
			   (cdr card)
			   (car card))))
      (observe! (eq? side-shown 'red))
      other-side))))










;Value 16: ((red . 2/3) (blue . 1/3))

;;; Consider the classic problem of a drunk near a cliff: A drunk
;;; starts one step away from the cliff, and steps either directly
;;; toward the cliff or directly away every time.  What's the
;;; probability that he will fall?

(define (drunken-step place)
  (discrete-select ((- place 1) 1/2) ((+ place 1) 1/2)))

(define (drunken-wander start)
  (if (<= start 0)
      'fall
      (drunken-wander (drunken-step start))))

(define (print-incrementally answer)
  (distribution/refine! answer)
  (pp `(minimum
	,(exact->inexact (distribution/datum-min-mass answer 'fall))
	maximum
	,(distribution/datum-max-mass answer 'fall)))
  (print-incrementally answer))

(print-incrementally
 (stochastic-thunk->distribution
  (lambda () (drunken-wander 1))))

(minimum .5 maximum 1)
(minimum .625 maximum 1)
(minimum .65625 maximum 1)
(minimum .6640625 maximum 1)
(minimum .666015625 maximum 1)
(minimum .66650390625 maximum 1)
(minimum .6666259765625 maximum 1)
(minimum .666656494140625 maximum 1)
(minimum .6666641235351562 maximum 1)
(minimum .6666660308837891 maximum 1)
(minimum .6666665077209473 maximum 1)
(minimum .6666666269302368 maximum 1)
(minimum .6666666567325592 maximum 1)
(minimum .6666666641831398 maximum 1)
(minimum .666666666045785 maximum 1)
(minimum .6666666665114462 maximum 1)
(minimum .6666666666278616 maximum 1)
(minimum .6666666666569654 maximum 1)
(minimum .6666666666642413 maximum 1)
(minimum .6666666666660603 maximum 1)
(minimum .6666666666665151 maximum 1)
(minimum .6666666666666288 maximum 1)
(minimum .6666666666666572 maximum 1)
(minimum .6666666666666643 maximum 1)
  C-c C-c^G;Quit!

(print-incrementally
 (stochastic-thunk->distribution
  (lambda () (drunken-wander 1))
  make-breadth-first-schedule))

(minimum .5 maximum 1)
(minimum .625 maximum 1)
(minimum .65625 maximum 1)
(minimum .6875 maximum 1)
(minimum .6953125 maximum 1)
(minimum .703125 maximum 1)
(minimum .7109375 maximum 1)
(minimum .71875 maximum 1)
(minimum .7265625 maximum 1)
(minimum .728515625 maximum 1)
(minimum .73046875 maximum 1)
(minimum .732421875 maximum 1)
(minimum .734375 maximum 1)
(minimum .736328125 maximum 1)
(minimum .73828125 maximum 1)
(minimum .740234375 maximum 1)
(minimum .7421875 maximum 1)
(minimum .744140625 maximum 1)
(minimum .74609375 maximum 1)
(minimum .748046875 maximum 1)
(minimum .75 maximum 1)
(minimum .751953125 maximum 1)
(minimum .75390625 maximum 1)
  C-c C-c^G;Quit!
