#lang racket/base

(provide
 (struct-out gc-info)
 make-gc-info-evt)

(struct gc-info
  (mode
   pre-amount
   pre-admin-amount
   code-amount
   post-amount
   post-admin-amount
   start-process-time
   end-process-time
   start-time
   end-time)
  #:prefab)

(define (make-gc-info-evt)
  (handle-evt
   (make-log-receiver (current-logger) 'debug 'GC)
   (λ (vs) (vector-ref vs 2))))
