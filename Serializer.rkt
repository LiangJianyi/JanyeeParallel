#lang racket
(require compatibility/mlist)
(provide parallel-execute)
(provide make-serializer)

(define (parallel-execute . proc)
  (define thd-lst null)
  (for ([p proc])
    (if [eq? thd-lst null]
        (set! thd-lst (list (thread p)))
        (set! thd-lst (append thd-lst (list (thread p))))))
  thd-lst)

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      (cons serialized-p mutex))))

(define (make-mutex)
  (let ((cell (mlist false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             ;; true if already set, false once set
             (if (test-and-set! cell) 
                 (the-mutex 'acquire) ;; retry, causing "blocking"
                 false)) 
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-mcar! cell false))

(define (test-and-set! cell)
  (if (mcar cell)
      true
      (begin (set-mcar! cell true)
             false)))
