#lang racket/base

(require racket/runtime-path)

(provide
 get-object-counts)

(define-runtime-module-path bc-mod "memory-bc.rkt")
(define-runtime-module-path cs-mod "memory-cs.rkt")
(define mod-path
  (case (system-type 'vm)
    [(chez-scheme) cs-mod]
    [else bc-mod]))

(define get-object-counts
  (dynamic-require mod-path 'get-object-counts))


;; GC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (struct-out gc-info)
 make-gc-info-evt)

(struct gc-info
  (mode pre-amount pre-admin-amount code-amount
        post-amount post-admin-amount
        start-process-time end-process-time
        start-time end-time)
  #:prefab)

(define (make-gc-info-evt)
  (handle-evt
   (make-log-receiver (current-logger) 'debug 'GC)
   (λ (vs) (vector-ref vs 2))))
