; factorial.scm
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

; Test cases
(display (factorial 5)) ; 120
(newline)
(display (factorial 0)) ; 1
(newline)
 