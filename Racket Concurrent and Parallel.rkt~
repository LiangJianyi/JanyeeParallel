#lang racket
(define global -1)

(define (f1)
  (letrec [[fuck (* global global)]
           [loop (lambda (arg)
                   (if [< arg 100000000]
                       (loop (+ arg 1))
                       arg))]]
    (loop fuck)))

(define (f2) (set! global (+ global 100)))

(define t1 (thread f1))
;(call-in-nested-thread f1 (current-custodian))
;(call-in-nested-thread f2 (current-custodian))

(define (f3)
  (define (f3-1)
    (+ (+ 1 2)
       (call-in-nested-thread (lambda () (+ (+ 1 3)
                                            (call-in-nested-thread (lambda () (+ 1 4))
                                                                   (current-custodian))))
                              (current-custodian))))
  (call-in-nested-thread f3-1 (current-custodian)))


(custodian-shut-down? (current-custodian))