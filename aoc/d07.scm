#!/usr/bin/guile \
-L .
!#

;; AoC 2019 Day 7

;; Problem 1

(define-module (aoc d07)
  #:export (amplifier-chain
            program-1))

(use-modules (ice-9 rdelim)
             (aoc intcode))

(define stdout (current-output-port))

(define (amplifier-chain p phases input)
  (let ((prog (vector-copy p))
        (in (open-input-string (string-join (list (car phases) input) "\n")))
        (out (open-output-string)))
    (set-current-input-port in)
    (set-current-output-port out)
    (run-program prog)
    (if (null? (cdr phases))
      (begin (set-current-output-port stdout) (get-output-string out))
      (amplifier-chain p (cdr phases) (string-trim (get-output-string out))))))

(define amplifer-program (parse-program (read-line (open-input-file "input/d07"))))
