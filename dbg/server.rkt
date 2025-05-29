#lang racket/base

(require racket/contract/base
         "private/server.rkt")

(provide
 (contract-out
  [serve (->* []
              [#:host string?
               #:port (integer-in 0 65535)
               #:custodian custodian?]
              (-> void?))]))
