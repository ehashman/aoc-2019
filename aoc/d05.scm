#!/usr/bin/guile \
-L .
!#

;; AoC 2019 Day 5

;; Problem 1

(define-module (aoc d05))

(use-modules (ice-9 rdelim)
             (aoc intcode))

;; Test input/output
; (run-program (vector 3 0 4 0 99))

(run-program (parse-program (read-line (open-input-file "input/d05"))))
