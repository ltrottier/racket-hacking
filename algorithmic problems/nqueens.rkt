#lang racket
(require racket/struct)

(struct Queen (row col)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'Queen)
      (lambda (obj) (list (Queen-row obj) (Queen-col obj)))))]
  )

(define (Queen-attacked-by? q1 q2)
  (define q1-row (Queen-row q1))
  (define q1-col (Queen-col q1))
  (define q2-row (Queen-row q2))
  (define q2-col (Queen-col q2))
  (define delta-col (- q1-col q2-col))
  (define delta-row (- q1-row q2-row))
  (define slope (/ delta-row delta-col))
  (define same-diag? (or (equal? slope 1) (equal? slope -1)))
  (define same-row? (eq? q1-row q2-row))
  (define same-col? (eq? q1-col q2-col))
  (or (or same-row? same-col?) same-diag?)
)

(define (Queen-move-at-row q1 row)
  (define q1-col (Queen-col q1))
  (Queen row q1-col)
)

(define (Queen-attacked-by-list? q1 q2-list)
  (define (rec q1 q2-list attacked?)
    (cond
      [(empty? q2-list) attacked?]
      [(eq? attacked? #t) attacked?]
      [else
       (define q2 (first q2-list))
       (define q2-attacks-q1? (Queen-attacked-by? q1 q2))
       (define other-queens (rest q2-list))
       (define attacked?-updated (or attacked? q2-attacks-q1?))
       (rec q1 other-queens attacked?-updated)
       ]
      )
    )
  (rec q1 q2-list #f)
)

(define (solution-verify solution)
  (define (rec solution valid?)
    (cond
      [(empty? solution) valid?]
      [(eq? valid? #f) valid?]
      [else
       (define attacked?
         (Queen-attacked-by-list?
          (first solution)
          (rest solution)))
       (define not-attacked? (not attacked?))
       (rec (rest solution) (and valid? not-attacked?))
       ])
    )
  (rec solution #t)
) 

(define (solution-next solution)
  (define n (length solution))
  (define (rec cur-sol new-sol)
    (cond
      [(empty? cur-sol) '()]
      [else
       (define q1 (first cur-sol))
       (define q1-row (Queen-row q1))
       (define q1-row-new (+ q1-row 1))
       (cond
         [(> q1-row-new n)
           (define q1-new (Queen-move-at-row q1 1))
           (define new-sol-updated (append new-sol (list q1-new)))
           (define cur-sol-updated (rest cur-sol))
           (rec cur-sol-updated new-sol-updated)
         ]
         [else
           (define q1-new (Queen-move-at-row q1 q1-row-new))
           (append new-sol (list q1-new) (rest cur-sol))
         ]
       )
      ]
    )
  )
  (rec solution '())
)

(define (solution-init n)
  (build-list n (lambda (i) (Queen 1 (+ i 1))))
)

(define (solution-print s)
  (define n (length s))
  (define (rec s cnt)
    (cond
      [(empty? s) cnt]
      [else
       (define q (first s))
       (define pos (Queen-row q))
       (define left (make-string (- pos 1) #\o))
       (define right (make-string (- n pos) #\o))
       (define cnt-new
         (string-append
          left (string #\x) right (string #\newline)))
       (rec (rest s) (string-append cnt cnt-new))
      ]
    )
  )
  (define cnt (rec s ""))
  (fprintf (current-output-port) cnt)
)
  
 

(define (nqueens n)
  (define (rec s)
    (define solution-valid? (solution-verify s))
    (cond
      [(eq? solution-valid? #t) s]
      [else
       (define s-next (solution-next s))
       (rec s-next)
       ]
      )
    )
  (define s0 (solution-init n))
  (rec s0)
)


(define solution (nqueens 5))