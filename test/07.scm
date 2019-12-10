#!/usr/bin/guile \
-L . --use-srfi=64
!#

(use-modules (ice-9 rdelim)
             (aoc d07))

(test-begin "puzzle-1")

(define program-1 (vector 3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0))

(define program-2 (vector 3 23 3 24 1002 24 10 24 1002 23 -1 23
                          101 5 23 23 1 24 23 23 4 23 99 0 0))

(define program-3 (vector 3 31 3 32 1002 32 10 32 1001 31 -2 31 1007 31 0 33
                          1002 33 7 33 1 33 31 31 1 32 31 31 4 31 99 0 0 0))

(test-equal "43210\n" (amplifier-chain program-1 (list "4" "3" "2" "1" "0") "0"))
(test-equal "54321\n" (amplifier-chain program-2 (list "0" "1" "2" "3" "4") "0"))
(test-equal "65210\n" (amplifier-chain program-3 (list "1" "0" "4" "3" "2") "0"))

(test-end "puzzle-1")
