;;; -*- mode: scheme -*-

(define mappings
  '((#\a . "2") (#\b . "22") (#\c . "222")
    (#\d . "3") (#\e . "33") (#\f . "333")
    (#\g . "4") (#\h . "44") (#\i . "444")
    (#\j . "5") (#\k . "55") (#\l . "555")
    (#\m . "6") (#\n . "66") (#\o . "666")
    (#\p . "7") (#\q . "77") (#\r . "777") (#\s . "7777")
    (#\t . "8") (#\u . "88") (#\v . "888")
    (#\w . "9") (#\x . "99") (#\y . "999") (#\z . "9999")
    (#\space . "0")))



(define (translate str)
  (join (map (lambda (ch)
               (cond ((assv ch mappings) => cdr)
                     (else "?")))
             (string->list str))))

(define (join strs)
  (let loop ((res "")
             (strs strs))
    (cond ((null? strs)
           res)
          ((char=? (string-first (car strs))
                   (string-last res))
           (loop (string-append res " " (car strs))
                 (cdr strs)))
          (else
           (loop (string-append res (car strs))
                 (cdr strs))))))

(define (string-first str)
  (define len (string-length str))
  (if (zero? len)
      (integer->char 0)
      (string-ref str 0)))

(define (string-last str)
  (define len (string-length str))
  (if (zero? len)
      (integer->char 0)
      (string-ref str (- len 1))))


(define input-1 "hi")
(define output-1 "44 444")

(unless (equal? output-1 (translate input-1))
  (error "FUUUUUUCK"))

(define input-2 "yes")
(define output-2 "999337777")

(unless (equal? output-2 (translate input-2))
  (error "FUUUUUCK"))

(define input-3 "foo bar")
(define output-3 "333666 666022 2777")

(unless (equal? output-3 (translate input-3))
  (error "FUUUUUCK"))

(define input-4 "hello world")
(define output-4 "4433555 555666096667775553")

(unless (equal? output-4 (translate input-4))
  (error "FUUUUUCK"))

