;;; -*- mode: scheme -*-

(define peeps
  '(foo bar baz quux eduardo flow maton sabrozo))

(define (exclusive-random idxs i len)
  (let try ((j (random len)))
    (if (or (= j i) (member j idxs =))
        (try (random len))
        j)))

(define (pair-peeps peeps)
  (define len (length peeps))
  (let loop ((mapping '())
             (idxs    '())
             (i       0)
             (rest    peeps))
    (cond ((null? rest)
           (reverse mapping))
          ((and (= (+ i 1) len)
                (not (member i idxs =)))
           (pair-peeps peeps))
          (else
           (let ((r (exclusive-random idxs i len)))
             (loop (cons (cons (car rest) (list-ref peeps r))
                         mapping)
                   (cons r idxs)
                   (+ i 1)
                   (cdr rest)))))))
