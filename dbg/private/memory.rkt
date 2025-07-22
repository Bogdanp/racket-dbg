#lang racket/base

(require racket/format
         racket/runtime-path)

(provide
 get-object-counts
 get-object-counts-by-module
 get-object-graph/by-type)

(define-runtime-module-path bc-mod "memory-bc.rkt")
(define-runtime-module-path cs-mod "memory-cs.rkt")
(define mod-path
  (case (system-type 'vm)
    [(chez-scheme) cs-mod]
    [else bc-mod]))

(define get-object-counts
  (dynamic-require mod-path 'get-object-counts))

(define get-object-counts-by-module
  (dynamic-require mod-path 'get-object-counts-by-module))

(define get-object-graph/by-type
  (dynamic-require mod-path 'get-object-graph/by-type))


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


;; Memory Hogs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 find-memory-hogs)

(define (find-memory-hogs limit cust super-cust)
  (define seen
    (make-hasheq))
  (let loop ([v cust])
    (cond
      [(hash-has-key? seen v) null]
      [else
       (hash-set! seen v #t)
       (cond
         [(custodian? v)
          (define total
            (current-memory-use v))
          (if (total . >= . limit)
              (list
               (hasheq
                'id (eq-hash-code v)
                'kind 'custodian
                'name (~s v)
                'total total
                'children (loop (custodian-managed-list cust super-cust))))
              null)]
         [(thread? v)
          (list
           (hasheq
            'id (eq-hash-code v)
            'kind 'thread
            'name (~s v)))]
         [(list? v)
          (apply append (map loop v))]
         [else
          null])])))
