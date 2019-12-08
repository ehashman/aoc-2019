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

(define (normalize-bool b)
  (if b 1 0))

;; Just support a pair of modes for now
(define (modes o)
  (let ((m (quotient o 100)))
    (cons (remainder m 10)
          (remainder (quotient m 10) 10))))

(define (get-value mode p l)
  (if (eqv? mode 1)
    l
    (vector-ref p l)))

(define (parse-program p)
  (list->vector (map string->number (string-split p #\,))))

(define (eval-inst i p f)
  (let* ((l1  (vector-ref p (+ i 1)))
         (l2  (vector-ref p (+ i 2)))
         (dst (vector-ref p (+ i 3)))
         (ms  (modes (vector-ref p i)))
         (m1  (car ms))
         (m2  (cdr ms))
         (v1  (get-value m1 p l1))
         (v2  (get-value m2 p l2)))
    (vector-set! p dst (f v1 v2))
    (+ i 4)))

(define (get-input i p)
  (let ((dst (vector-ref p (+ i 1)))
        (input (string->number (read-line))))
    (vector-set! p dst input)
    (+ i 2)))

(define (write-output i p)
  (let* ((dst (vector-ref p (+ i 1)))
         (ms  (modes (vector-ref p i)))
         (m1  (car ms)))
    (display (get-value m1 p dst))
    (newline)
    (+ i 2)))

(define (jump i p pred)
  (let* ((l1 (vector-ref p (+ i 1)))
         (l2 (vector-ref p (+ i 2)))
         (ms (modes (vector-ref p i)))
         (m1 (car ms))
         (m2 (cdr ms))
         (v1 (get-value m1 p l1))
         (v2 (get-value m2 p l2)))
    (if (pred v1)
      v2
      (+ i 3))))

(define (run-inst i p)
  (case (remainder (vector-ref p i) 100)
    ((1) (eval-inst i p +))
    ((2) (eval-inst i p *))
    ((3) (get-input i p))
    ((4) (write-output i p))
    ((5) (jump i p (negate zero?)))
    ((6) (jump i p zero?))
    ((7) (eval-inst i p (compose normalize-bool <)))
    ((8) (eval-inst i p (compose normalize-bool eqv?)))
    ((99) -1)))

(define (run-program p)
  (do ((i 0 (run-inst i p)))
      ((eqv? i -1) p)))
