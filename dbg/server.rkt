#lang racket/base

(require racket/contract
         "private/server.rkt")

(provide
 (contract-out
  [serve/install-custodian (->* ()
                                (#:host string?
                                 #:port (integer-in 0 65535))
                                (-> void?))]
  [serve (->* ()
              (#:host string?
               #:port (integer-in 0 65535))
              (-> void?))]))
