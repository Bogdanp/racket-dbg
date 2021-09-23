#lang racket/base

(require racket/contract
         "private/client.rkt"
         "private/gc.rkt")

(provide
 (struct-out gc-info)
 (contract-out
  [current-client (parameter/c client?)]
  [client? (-> any/c boolean?)]
  [connect (->* ()
                (#:host string?
                 #:port (integer-in 0 65535))
                client?)]
  [disconnect (client-> void?)]
  [subscribe (case-client-> symbol? void?)]
  [unsubscribe (case-client-> symbol? void?)]
  [async-evt (client-> evt?)]
  [get-info (client-> hash?)]
  [get-memory-use (client-> exact-positive-integer?)]
  [get-managed-item-counts (client-> (hash/c symbol? exact-positive-integer?))]))

(define-syntax-rule (client-> arg/c ... res/c)
  (->* (arg/c ...) (client?) res/c))

(define-syntax-rule (case-client-> arg/c ... res/c)
  (case->
   (-> arg/c ... res/c)
   (-> client? arg/c ... res/c)))
