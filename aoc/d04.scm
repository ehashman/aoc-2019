#!/usr/bin/guile
!#

;; AoC 2019 Day 4

;; Problem 1

(define-module (aoc d04)
  #:export (rule-3
            rule-3b
            rule-4))

(use-modules (srfi srfi-1))

(define lower-input 254032)
(define upper-input 789860)

(define (get-digits n)
  (map (compose string->number string)
       (string->list (number->string n))))

;; By only considering input between {lower,upper}-input, we already
;; satisfy both these rules.

;; Rule 1: 6 digit length
;; (define rule-1 (lambda (n) (and (> n 99999) (< n 1000000))))

;; Rule 2: Must be within input bounds
;; (define rule-2 (lambda (n) (and (> n lower-input) (< n upper-input))))

;; Rule 3: Must have two repeated digits
(define (rule-3 n)
  (cdr (fold (lambda (l r)
               (cons l (or (eq? l (car r)) (cdr r))))
             '(0 . #f)
             (get-digits n))))

;; Rule 3b: If it has repeated digits, must have a group of at most 2
(define (rule-3b n)
  (let* ((digits (get-digits n))
         (num-digits (lambda (digit) (count (lambda (x) (eqv? x digit)) digits))))
    (list? (memv 2 (map num-digits digits)))))  ; coerce matched list to bool

;; Rule 4: Digits must be in ascending order
(define (rule-4 n)
  (cdr (fold (lambda (l r)
               (cons l (and (>= l (car r)) (cdr r))))
             '(0 . #t)
             (get-digits n))))

(display
 (length (filter (lambda (x) (and (rule-3b x) (rule-4 x)))
                 (iota (- upper-input lower-input) lower-input))))
(newline)
