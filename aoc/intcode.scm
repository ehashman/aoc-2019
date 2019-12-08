#!/usr/bin/guile
!#

;; Intcode module

(define-module (aoc intcode)
  #:export (parse-program
            eval-inst
            run-inst
            run-program))

(define (parse-program p)
  (list->vector (map string->number (string-split p #\,))))

(define (eval-inst i p f)
  (begin
    (let* ((l1  (vector-ref p (+ i 1)))
           (l2  (vector-ref p (+ i 2)))
           (dst (vector-ref p (+ i 3)))
           (v1  (vector-ref p l1))
           (v2  (vector-ref p l2)))
      (vector-set! p dst (f v1 v2)))
    (+ i 4)))

(define (run-inst i p)
  (case (vector-ref p i)
    ((1) (eval-inst i p +))
    ((2) (eval-inst i p *))
    ((99) -1)))

(define (run-program p)
  (do ((i 0 (run-inst i p)))
      ((eqv? i -1) p)))
