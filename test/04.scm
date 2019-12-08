#!/usr/bin/guile \
-L . --use-srfi=64
!#

(use-modules (aoc d04))

(test-begin "puzzle-1")

(test-eq (rule-3 11234) #t)
(test-eq (rule-3 122345) #t)
(test-eq (rule-3 111111) #t)
(test-eq (rule-3 12345) #f)
(test-eq (rule-3 123789) #f)

(test-eq (rule-4 12345) #t)
(test-eq (rule-4 111123) #t)
(test-eq (rule-4 135679) #t)
(test-eq (rule-4 111111) #t)
(test-eq (rule-4 123789) #t)
(test-eq (rule-4 12354) #f)
(test-eq (rule-4 223450) #f)

(test-end "puzzle-1")

(test-begin "puzzle-2")

(test-eq (rule-3b 112233) #t)
(test-eq (rule-3b 111122) #t)
(test-eq (rule-3b 111111) #f)
(test-eq (rule-3b 123444) #f)

(test-end "puzzle-2")
