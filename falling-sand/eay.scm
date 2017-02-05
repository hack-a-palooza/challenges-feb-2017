;;; -*- mode: scheme -*-

(define (parse-line str)
  (map (lambda (ch)
         (case ch
           ((#\.) 'sand)
           ((#\#) 'block)
           ((#\_) 'blank)))
       (string->list str)))

(define (sand-array . strs)
  (let recur ((rows (map parse-line strs)))
    (if (every null? rows)
        '()
        (cons (map car rows)
              (recur (map cdr rows))))))

(define (solve-col col)
  (let loop ((xs  (list (car col)))
             (col (cdr col)))
    (cond ((null? col)
           xs)
          ((eq? (car col) 'block)
           (append xs col))
          (else
           (loop (cons (car col) xs)
                 (cdr col))))))

(define (solve cols)
  (map solve-col cols))

(define input-1
  (sand-array "...."
              "____"
              "____"
              "____"))

(define output-1
  (sand-array "____"
              "____"
              "____"
              "...."))

(unless (equal? output-1 (solve input-1))
  (error "FUCK!"))

(define input-2
  (sand-array "...."
              "_#__"
              "___#"
              "#___"))

(define output-2
  (sand-array "_.__"
              "_#_."
              ".__#"
              "#_._"))

(unless (equal? output-2 (solve input-2))
  (error "FUCK!"))

(define input-3
  (sand-array "..."
              "##_"
              "#__"))

(define output-3
  (sand-array ".._"
              "##_"
              "#_."))

(unless (equal? output-3 (solve input-3))
  (error "FUCK!"))
