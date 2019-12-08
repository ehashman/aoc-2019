#!/usr/bin/guile \
-L . --use-srfi=64
!#

(use-modules (aoc d01))

(test-begin "puzzle-1")

(test-equal (fuel-required 12) 2)
(test-equal (fuel-required 14) 2)
(test-equal (fuel-required 1969) 654)
(test-equal (fuel-required 100756) 33583)

(test-end "puzzle-1")

(test-begin "puzzle-2")

(test-equal (corrected-fuel-required 14) 2)
(test-equal (corrected-fuel-required 1969) 966)
(test-equal (corrected-fuel-required 100756) 50346)

(test-end "puzzle-2")
