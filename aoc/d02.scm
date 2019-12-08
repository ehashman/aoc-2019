#!/usr/bin/guile
!#

;; AoC 2019 Day 2

(define-module (aoc d02)
  #:export (parse-program
            eval-inst
            run-inst))

(use-modules (ice-9 rdelim))

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
    #t))

(define (run-inst i p)
  (case (vector-ref p i)
    ((1) (eval-inst i p +))
    ((2) (eval-inst i p *))
    ((99) #f)))

(set-current-input-port (open-input-file "input/d02"))
