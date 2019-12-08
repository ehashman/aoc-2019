#!/usr/bin/guile \
-L . --use-srfi=64
!#

(use-modules (ice-9 rdelim)
             (aoc intcode))

(test-begin "intcode-checker")

(test-equal (parse-program "1,0,0,0,99") #(1 0 0 0 99))
(test-equal (parse-program "1,1,1,4,99,5,6,0,99")
            #(1 1 1 4 99 5 6 0 99))

(define program #(1 9 10 3 2 3 11 0 99 30 40 50))

(define p (vector-copy program))

; i = 0, opcode is 1, add
(test-eq (eval-inst 0 p +) 4)
(test-equal p #(1 9 10 70 2 3 11 0 99 30 40 50))

; i = 4, opcode is 2, multiply
(test-eq (eval-inst 4 p *) 8)
(test-equal p #(3500 9 10 70 2 3 11 0 99 30 40 50))

; test sample programs
(set! p (vector 1 0 0 0 99))
(run-program p)
(test-equal p #(2 0 0 0 99))

(set! p (vector 2 3 0 3 99))
(run-program p)
(test-equal p #(2 3 0 6 99))

(set! p (vector 2 4 4 5 99 0))
(run-program p)
(test-equal p #(2 4 4 5 99 9801))

(set! p (vector 1 1 1 4 99 5 6 0 99))
(run-program p)
(test-equal p #(30 1 1 4 2 5 6 0 99))

(set! p (vector-copy program))
(run-program p)
(test-equal p #(3500 9 10 70 2 3 11 0 99 30 40 50))

; test input
(set! p (vector 3 0 99))
(set-current-input-port (open-input-string "666"))
(run-program p)
(test-equal p #(666 0 99))

; test output
(define stdout (current-output-port))
(define my-output (open-output-string))
(set-current-output-port my-output)
(write-output 0 #(4 3 99 666))
(set-current-output-port stdout)
(test-equal (get-output-string my-output) "666\n")

; test mode switching
(set! p (vector 1002 4 3 4 33))
(run-program p)
(test-equal p #(1002 4 3 4 99))

; test negative ints
(set! p (vector 1101 100 -1 4 0))
(run-program p)
(test-equal p #(1101 100 -1 4 99))

; test immediate mode for output
(set! my-output (open-output-string))
(set-current-output-port my-output)
(write-output 0 #(104 666 99))
(set-current-output-port stdout)
(test-equal (get-output-string my-output) "666\n")

; more sample programs
(define (test-program-with-io p input output)
  (set-current-input-port (open-input-string input))
  (set! my-output (open-output-string))
  (set-current-output-port my-output)
  (run-program p)
  (set-current-output-port stdout)
  (test-equal (get-output-string my-output) output))

(test-program-with-io (vector 3 9 8 9 10 9 4 9 99 -1 8) "8" "1\n")
(test-program-with-io (vector 3 9 8 9 10 9 4 9 99 -1 8) "80" "0\n")
(test-program-with-io (vector 3 9 7 9 10 9 4 9 99 -1 8) "3" "1\n")
(test-program-with-io (vector 3 9 7 9 10 9 4 9 99 -1 8) "8" "0\n")
(test-program-with-io (vector 3 3 1108 -1 8 3 4 3 99) "8" "1\n")
(test-program-with-io (vector 3 3 1108 -1 8 3 4 3 99) "80" "0\n")
(test-program-with-io (vector 3 3 1107 -1 8 3 4 3 99) "3" "1\n")
(test-program-with-io (vector 3 3 1107 -1 8 3 4 3 99) "8" "0\n")
(test-program-with-io (vector 3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9) "8" "1\n")
(test-program-with-io (vector 3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9) "0" "0\n")
(test-program-with-io (vector 3 3 1105 -1 9 1101 0 0 12 4 12 99 1) "8" "1\n")
(test-program-with-io (vector 3 3 1105 -1 9 1101 0 0 12 4 12 99 1) "0" "0\n")

(define long-p #(3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31
                 1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104
                 999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99))
(test-program-with-io (vector-copy long-p) "5" "999\n")
(test-program-with-io (vector-copy long-p) "8" "1000\n")
(test-program-with-io (vector-copy long-p) "20" "1001\n")


(test-end "intcode-checker")
