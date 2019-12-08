;; AoC 2019 Day 1

(define-module (aoc d01)
  #:export (fuel-required))

(define (fuel-required mass)
  (- (quotient mass 3) 2))
