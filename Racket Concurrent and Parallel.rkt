#lang racket
(require compatibility/mlist)
(define global -1)
(define cust (make-custodian (current-custodian)))

(define (f1)
  (letrec [[fuck (* global global)]
           [loop (lambda (arg)
                   (if [< arg 100000000]
                       (loop (+ arg 1))
                       arg))]]
    (loop fuck)))

(define (f2) (set! global (+ global 100)))

(define t1 (thread f1))
(define t2 (thread f2))
;(call-in-nested-thread f1 cust)
;(call-in-nested-thread f2 cust)

(define (f3)
  (define (f3-1)
    (+ (+ 1 2)
       (call-in-nested-thread (lambda () (+ (+ 1 3)
                                            (call-in-nested-thread (lambda () (+ 1 4))
                                                                   cust)))
                              cust)))
  (call-in-nested-thread f3-1 cust))
;(f3)
(define t3 (thread f3))

(sync t1 t2 t3)

'------------------------
(define custbox (make-custodian-box (current-custodian) global))
(custodian-box? custbox)
(custodian-box-value custbox)

'------------------------
(letrec [[get-random-number (lambda () (random -999 999))]
      [mlst (mlist 'start)]
      [f1 (lambda ()
            (do [[x 0 (get-random-number)]]
              [[equal? x 1] [mappend! mlst (mlist x)]
                            (displayln x)]))]
      [f2 (lambda ()
            (do [[x 0 (get-random-number)]]
              [[equal? x 2] [mappend! mlst (mlist x)]
                            (displayln x)]))]
      [f3 (lambda ()
            (do [[x 0 (get-random-number)]]
              [[equal? x 3] [mappend! mlst (mlist x)]
                            (displayln x)]))]
      [f4 (lambda ()
            (do [[x 0 (get-random-number)]]
              [[equal? x 4] [mappend! mlst (mlist x)]
                            (displayln x)]))]]
  (let [[t1 (thread f1)]
        [t2 (thread f2)]
        [t3 (thread f3)]
        [t4 (thread f4)]]
    (begin (sync t1 t2 t3 t4)
           mlst
           (kill-thread t1)
           (kill-thread t2)
           (kill-thread t3)
           (kill-thread t4))))


'-------------------------
;(define ch (make-channel))
;(thread (λ () (displayln (sync ch))))
;(channel-put ch (lambda () (+ 1 2)))

(define t5 (thread (lambda () (displayln "This is a new thread."))))
(define t6 (thread (lambda () (displayln "This is another new thread."))))

(define worker (thread (lambda ()
                         (let loop ([x 0])
                           (begin (display "Working with ")
                                  (displayln x))
                           (if [= x 10]
                               (begin (display (current-thread))
                                      (displayln "is done.")
                                      (kill-thread (current-thread)))
                               (loop [+ x 1]))))))

(kill-thread t5)
(kill-thread t6)
(thread-wait worker)
(if [thread-dead? worker]
    (displayln "worker was dead.")
    (kill-thread worker))

'-------------------------
(define (parallel-example)
  (letrec [[x 0]
           [func1 (lambda ()
                    (fprintf (current-output-port) "~a value before execute func1 is ~a\n" "x" x)
                    (set! x (+ x 1))
                    (fprintf (current-output-port) "~a value after execute func1 is ~a\n" "x" x))]
           [func2 (lambda ()
                    (fprintf (current-output-port) "~a value before execute func2 is ~a\n" "x" x)
                    (set! x (* x x))
                    (fprintf (current-output-port) "~a value after execute func2 is ~a\n" "x" x))]
           [func3 (lambda ()
                    (fprintf (current-output-port) "~a value before execute func3 is ~a\n" "x" x)
                    (set! x (+ x 0.2222))
                    (fprintf (current-output-port) "~a value after execute func3 is ~a\n" "x" x))]]
    (synchronous-execute func1 func2 func3)))

(custodian-shutdown-all cust)

(define (synchronous-execute . proc)
  (for ([p proc])
    (thread p))) ;;; 使用 thread 而不是直接执行 (p) 时,3个fuck将会随机同步执行而不是线性执行

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "Serializer.rkt")
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
(define s (make-serializer))
(parallel-execute (s fuck1)
                  (s fuck2)
                  (s fuck3))

'-------------------------
