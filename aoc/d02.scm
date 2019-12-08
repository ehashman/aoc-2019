#!/usr/bin/guile
!#

;; AoC 2019 Day 2

(define-module (aoc d02)
  #:export (parse-program))

(use-modules (ice-9 rdelim))

(define (parse-program program)
  (list->vector (map string->number (string-split program #\,))))

(set-current-input-port (open-input-file "input/d02"))
