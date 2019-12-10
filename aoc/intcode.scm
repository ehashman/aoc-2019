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

;;;; Helper functions ;;;;
(define (normalize-bool b)
  (if b 1 0))

(define (modes o)
  (let ((m (quotient o 100)))
    ; just support a pair of modes for now
    (cons (remainder m 10)
          (remainder (quotient m 10) 10))))

(define (get-value mode p l)
  (let ((len (vector-length p)))
    (case mode
      ((0) (if (>= l len) (hash-ref (vector-ref p (1- len)) l 0)
                          (vector-ref p l)))
      ((1) l))))

(define (set-value p l v)
  (let ((len (vector-length p)))
    (if (>= l len)
      (hash-set! (vector-ref p (1- len)) l v)
      (vector-set! p l v))))

;;;; Run code ;;;;
(define (eval-inst i p f)
  (let* ((l1  (vector-ref p (+ i 1)))
         (l2  (vector-ref p (+ i 2)))
         (dst (vector-ref p (+ i 3)))
         (ms  (modes (vector-ref p i)))
         (m1  (car ms))
         (m2  (cdr ms))
         (v1  (get-value m1 p l1))
         (v2  (get-value m2 p l2)))
    (set-value p dst (f v1 v2))
    (+ i 4)))

(define (get-input i p)
  (let ((dst (vector-ref p (+ i 1)))
        (input (string->number (read-line))))
    (set-value p dst input)
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

(define (actually-run-program p)
  (do ((i 0 (run-inst i p)))
      ((eqv? i -1) (drop-memory p))))

;;;; Some paperwork ;;;;
(define (prepare-program p)
  (let* ((l  (vector-length p))
         (p0 (make-vector (1+ l))))
    (vector-move-left! p 0 l p0 0)
    (vector-set! p0 l (make-hash-table)) ;; add some RAM
    p0))

(define (drop-memory p)
  (let* ((l  (vector-length p))
         (p0 (make-vector (1- l))))
    (when (hash-table? (vector-ref p (1- l)))  ;; drop the RAM, if it exists
      (vector-move-left! p 0 (1- l) p0 0)
      p0)))

(define (run-program p)
  (actually-run-program (prepare-program p)))
