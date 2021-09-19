#lang racket/base

(provide
 write/flush)

(define (write/flush data out)
  (write data out)
  (flush-output out))
