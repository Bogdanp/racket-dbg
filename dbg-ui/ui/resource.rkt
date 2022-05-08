#lang racket/base

(require racket/gui
         racket/runtime-path)

(provide
 icon)

(define-runtime-path resources-path "resources")

(define backing-scale
  (get-display-backing-scale))

(define (load-bitmap id)
  (define path (build-path resources-path (format "~a.png" id)))
  (read-bitmap path #:backing-scale backing-scale))

(define icon
  (let ([cache (make-weak-hasheq)])
    (lambda (id)
      (hash-ref! cache id (λ () (load-bitmap id))))))
