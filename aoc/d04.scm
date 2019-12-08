#!/usr/bin/guile
!#

;; AoC 2019 Day 4

;; Problem 1

(define-module (aoc d04)
  #:export (rule-3
            rule-4))

(use-modules (srfi srfi-1))

(define lower-input 254032)
(define upper-input 789860)

(define (get-digits n)
  (map (compose string->number string) (string->list n)))

;; (define rule-1 (lambda (n) (and (> n 99999) (< n 1000000))))
;; (define rule-2 (lambda (n) (and (> n lower-input) (< n upper-input))))

(define (rule-3 n)
  (cdr (fold (lambda (l r)
               (cons l (or (eq? l (car r)) (cdr r))))
             '(0 . #f)
             (get-digits n))))

(define (rule-4 n)
  (cdr (fold (lambda (l r)
               (cons l (and (>= l (car r)) (cdr r))))
             '(0 . #t)
             (get-digits n))))
