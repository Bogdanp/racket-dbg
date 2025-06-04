#lang racket/base

(require racket/contract/base
         "private/client.rkt"
         (except-in "private/memory.rkt"
                    get-object-counts
                    get-object-counts-by-module))

(provide
 (struct-out gc-info)
 (contract-out
  [current-client (parameter/c client?)]
  [client? (-> any/c boolean?)]
  [connect (->* ()
                (#:host string?
                 #:port (integer-in 0 65535))
                client?)]
  [connected? (client-> boolean?)]
  [reconnect! (client-> void?)]
  [disconnect! (client-> void?)]
  [subscribe (case-client-> symbol? void?)]
  [unsubscribe (case-client-> symbol? void?)]
  [async-evt (client-> evt?)]
  [get-info (client-> hash?)]
  [get-memory-use (client-> exact-positive-integer?)]
  [get-object-counts (client-> (listof (cons/c string? (cons/c exact-nonnegative-integer?
                                                               exact-nonnegative-integer?))))]
  [get-object-counts-by-module (client-> symbol? (listof (cons/c string? exact-nonnegative-integer?)))]
  [get-reference-graph (client-> symbol? hash?)]
  [start-profile (->* [] [client? exact-nonnegative-integer? boolean?] void?)]
  [stop-profile (client-> any/c)]
  [get-profile (client-> any/c)]
  [dump-threads (client-> string?)]))

(define-syntax-rule (client-> arg/c ... res/c)
  (->* (arg/c ...) (client?) res/c))

(define-syntax-rule (case-client-> arg/c ... res/c)
  (case->
   (-> arg/c ... res/c)
   (-> client? arg/c ... res/c)))
