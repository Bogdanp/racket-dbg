#lang racket/base

(provide
 write/flush)

(define (write/flush data [out (current-output-port)])
  (write data out)
  (flush-output out))
