#!/usr/bin/guile \
-L .
!#

;; AoC 2019 Day 9

;; Problem 1

(define-module (aoc d09))

(use-modules (ice-9 rdelim)
             (aoc intcode))

(define p (parse-program (read-line (open-input-file "input/d09"))))
(set-current-input-port (open-input-string "1"))
(run-program p)
(newline)

;; Problem 2

(set-current-input-port (open-input-string "2"))
(run-program p)
(newline)
