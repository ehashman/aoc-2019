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
      (amplifier-chain p (cdr phases) (get-output-string out)))))

(define amplifier-program (parse-program (read-line (open-input-file "input/d07"))))

;; There is no stdlib for combinatorics?! Lift it from RosettaCode I guess...
;; https://rosettacode.org/wiki/Permutations#Scheme

(define (insert l n e)
  (if (= 0 n)
      (cons e l)
      (cons (car l)
            (insert (cdr l) (- n 1) e))))

(define (seq start end)
  (if (= start end)
      (list end)
      (cons start (seq (+ start 1) end))))

(define (permute l)
  (if (null? l)
      '(())
      (apply append (map (lambda (p)
                           (map (lambda (n)
                                  (insert p n (car l)))
                                (seq 0 (length p))))
                         (permute (cdr l))))))
;;; /RosettaCode

(define sequences (permute (list "0" "1" "2" "3" "4")))

(define (run-amps phases)
  (amplifier-chain amplifier-program phases "0"))

(display
  (apply max (map (compose string->number string-trim-right run-amps) sequences)))
(newline)
