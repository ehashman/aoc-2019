#!/usr/bin/guile
!#

;; Intcode module

(define-module (aoc intcode)
  #:export (parse-program
            eval-inst
            run-inst
            run-program
            write-output))

(use-modules (ice-9 rdelim))

(define (parse-program p)
  (list->vector (map string->number (string-split p #\,))))

(define (eval-inst i p f)
  (let* ((l1  (vector-ref p (+ i 1)))
         (l2  (vector-ref p (+ i 2)))
         (dst (vector-ref p (+ i 3)))
         (modes (quotient (vector-ref p i) 100))
         (m1  (remainder modes 10))
         (m2  (remainder (quotient modes 10) 10))
         (v1  (if (eqv? m1 1) l1 (vector-ref p l1)))
         (v2  (if (eqv? m2 1) l2 (vector-ref p l2))))
    (vector-set! p dst (f v1 v2))
    (+ i 4)))

(define (get-input i p)
  (let ((dst (vector-ref p (+ i 1)))
        (input (string->number (read-line))))
    (vector-set! p dst input)
    (+ i 2)))

(define (write-output i p)
  (let* ((dst (vector-ref p (+ i 1)))
         (modes (quotient (vector-ref p i) 100))
         (m1  (remainder modes 10)))
    (display (if (eqv? m1 1) dst (vector-ref p dst)))
    (newline)
    (+ i 2)))

(define (run-inst i p)
  (case (remainder (vector-ref p i) 100)
    ((1) (eval-inst i p +))
    ((2) (eval-inst i p *))
    ((3) (get-input i p))
    ((4) (write-output i p))
    ((99) -1)))

(define (run-program p)
  (do ((i 0 (run-inst i p)))
      ((eqv? i -1) p)))
