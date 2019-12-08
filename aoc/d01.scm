#!/usr/bin/guile
!#

;; AoC 2019 Day 1

(define-module (aoc d01)
  #:export (fuel-required))

(use-modules (ice-9 rdelim))

(define (fuel-required mass)
  (- (quotient mass 3) 2))

(set-current-input-port (open-input-file "input/d01"))
(let lp ((mod-mass (read-line))
         (total-weight 0))
  (if (eof-object? mod-mass)
    (begin (display total-weight) (newline))
    (lp (read-line)
        (+ total-weight (fuel-required (string->number mod-mass))))))
