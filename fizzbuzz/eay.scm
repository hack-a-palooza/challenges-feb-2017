;;; -*- mode: scheme -*-

(define (divides? m i)
  (zero? (modulo i m)))

(define (fizzbuzz n)
  (do ((i 1 (+ i 1)))
      ((> i n) 'done)
    (cond ((and (divides? 3 i)
                (divides? 5 i))
           (display "fizzbuzz"))
          ((divides? 3 i)
           (display "fizz"))
          ((divides? 5 i)
           (display "buzz"))
          (else
           (display i)))
    (display " "))
  (newline))
