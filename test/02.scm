#!/usr/bin/guile \
-L . --use-srfi=64
!#

(use-modules (aoc d02))

(test-begin "puzzle-1")

(test-equal (parse-program "1,0,0,0,99") #(1 0 0 0 99))
(test-equal (parse-program "1,1,1,4,99,5,6,0,99")
            #(1 1 1 4 99 5 6 0 99))

(define program #(1 9 10 3 2 3 11 0 99 30 40 50))

(define p (vector-copy program))

; i = 0, opcode is 1, add
(test-eq (eval-inst 0 p +) #t)
(test-equal p #(1 9 10 70 2 3 11 0 99 30 40 50))

; i = 4, opcode is 2, multiply
(test-eq (eval-inst 4 p *) #t)
(test-equal p #(3500 9 10 70 2 3 11 0 99 30 40 50))

(test-end "puzzle-1")
