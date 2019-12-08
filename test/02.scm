#!/usr/bin/guile \
-L . --use-srfi=64
!#

(use-modules (aoc d02))

(test-begin "puzzle-1")

(test-equal (parse-program "1,0,0,0,99") #(1 0 0 0 99))
(test-equal (parse-program "1,1,1,4,99,5,6,0,99")
            #(1 1 1 4 99 5 6 0 99))

(test-end "puzzle-1")
