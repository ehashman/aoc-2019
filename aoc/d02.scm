#!/usr/bin/guile \
-L .
!#

;; AoC 2019 Day 2

;; Problem 1

(define-module (aoc d02))

(use-modules (ice-9 rdelim)
             (aoc intcode))

(set-current-input-port (open-input-file "input/d02"))
(define gravity-assist (parse-program (read-line)))
(display (run-program (vector-copy gravity-assist)))
(newline)


;; Problem 2

(define (check-inputs n v)
  (let ((p (vector-copy gravity-assist)))
    (vector-set! p 1 n)
    (vector-set! p 2 v)
    (eqv? (vector-ref (run-program p) 0) 19690720)))

;; who needs to break early, meh
(do ((n 0 (1+ n)))
    ((eqv? n 100))
  (do ((v 0 (1+ v)))
      ((eqv? v 100))
    (if (check-inputs n v)
      (display (+ (* n 100) v)))))
(newline)
