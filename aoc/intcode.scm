#!/usr/bin/guile
!#

;; Intcode module

(define-module (aoc intcode)
  #:export (parse-program
            eval-inst
            run-inst
            run-program
            write-output))

(use-modules (ice-9 rdelim)
             (srfi srfi-1))

(define (parse-program p)
  (list->vector (map string->number (string-split p #\,))))

;;;; Helper functions ;;;;
(define (normalize-bool b)
  (if b 1 0))

(define (modes o)
  (let ((m (quotient o 100)))
    (list (remainder m 10)
          (remainder (quotient m 10) 10)
          (remainder (quotient m 100) 10))))

(define (address-in-ram? len l ram)
  (and (>= l (1- len)) (hash-table? ram)))

(define (get-value mode p l rb)
  (let* ((len (vector-length p))
         (ram (vector-ref p (1- len)))
         (fetch-val
          (lambda (p len l ram)
            (if (address-in-ram? len l ram)
              (hash-ref ram l 0)
              (vector-ref p l)))))
    (case mode
      ((0) (fetch-val p len l ram))
      ((1) l)
      ((2) (fetch-val p len (+ l rb) ram)))))

(define (set-value mode p l v rb)
  (let* ((len (vector-length p))
         (ram (vector-ref p (1- len)))
         (set-val
          (lambda (p len l v ram)
            (if (address-in-ram? len l ram)
              (hash-set! ram l v)
              (vector-set! p l v)))))
    (case mode
      ((0) (set-val p len l v ram))
      ((2) (set-val p len (+ l rb) v ram)))))

;;;; Run code ;;;;
(define (eval-inst i p f)
  (let* ((ip  (car i))
         (rb  (cdr i))
         (l1  (vector-ref p (+ ip 1)))
         (l2  (vector-ref p (+ ip 2)))
         (dst (vector-ref p (+ ip 3)))
         (ms  (modes (vector-ref p ip)))
         (m1  (first ms))
         (m2  (second ms))
         (m3  (third ms))
         (v1  (get-value m1 p l1 rb))
         (v2  (get-value m2 p l2 rb)))
    (set-value m3 p dst (f v1 v2) rb)
    (cons (+ ip 4) rb)))

(define (get-input i p)
  (let* ((ip  (car i))
         (rb  (cdr i))
         (dst (vector-ref p (+ ip 1)))
         (ms  (modes (vector-ref p ip)))
         (m1  (car ms))
         (input (string->number (read-line))))
    (set-value m1 p dst input rb)
    (cons (+ ip 2) rb)))

(define (write-output i p)
  (let* ((ip  (car i))
         (rb  (cdr i))
         (dst (vector-ref p (+ ip 1)))
         (ms  (modes (vector-ref p ip)))
         (m1  (car ms)))
    (display (get-value m1 p dst rb))
    (newline)
    (cons (+ ip 2) rb)))

(define (jump i p pred)
  (let* ((ip (car i))
         (rb (cdr i))
         (l1 (vector-ref p (+ ip 1)))
         (l2 (vector-ref p (+ ip 2)))
         (ms (modes (vector-ref p ip)))
         (m1 (first ms))
         (m2 (second ms))
         (v1 (get-value m1 p l1 rb))
         (v2 (get-value m2 p l2 rb)))
    (if (pred v1)
      (cons v2 rb)
      (cons (+ ip 3) rb))))

(define (set-rb i p)
  (let* ((ip  (car i))
         (rb  (cdr i))
         (dst (vector-ref p (+ ip 1)))
         (ms  (modes (vector-ref p ip)))
         (m1  (car ms)))
    (cons (+ ip 2)
          (+ (get-value m1 p dst rb) rb))))

(define (run-inst i p)
  (case (remainder (vector-ref p (car i)) 100)
    ((1) (eval-inst i p +))
    ((2) (eval-inst i p *))
    ((3) (get-input i p))
    ((4) (write-output i p))
    ((5) (jump i p (negate zero?)))
    ((6) (jump i p zero?))
    ((7) (eval-inst i p (compose normalize-bool <)))
    ((8) (eval-inst i p (compose normalize-bool eqv?)))
    ((9) (set-rb i p))
    ((99) -1)))

(define (actually-run-program p)
  (do ((i '(0 . 0) (run-inst i p)))
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
