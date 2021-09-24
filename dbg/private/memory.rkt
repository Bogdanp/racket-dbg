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
