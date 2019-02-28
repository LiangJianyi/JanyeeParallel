#lang racket
(define (fuck1)
  (displayln "fuck1 executing...")
  (do []
    ([= (random -999999 999999) 1] [displayln "fuck1 result is 1"])))
(define (fuck2)
  (displayln "fuck2 executing...")
  (do []
    ([= (random -999999 999999) 2] [displayln "fuck2 result is 2"])))
(define (fuck3)
  (displayln "fuck3 executing...")
  (do []
    ([= (random -999999 999999) 3] [displayln "fuck3 result is 3"])))

(define (make-lock)
  (define current-index 0)
  (define (the-lock index)
    (cond [[> index current-index]
           (the-lock)]
          [[= index current-index]
           'execute]
          [[< index current-index]
           'release]))
  the-lock)

(define (make-task-constructor)
  (letrec [[current-index -1]
           [lock (make-lock)]]
    (lambda (thunk)
      (set! current-index (+ current-index 1))
      (lambda ()
        (when [eq? 'execute (lock current-index)]
          (thunk))
        (lock current-index)))))

(define make-task (make-task-constructor))
(define task1 (make-task fuck1))
(define task2 (make-task fuck2))
(define task3 (make-task fuck3))