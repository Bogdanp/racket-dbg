#lang racket/base

(provide
 get-object-counts
 get-object-graph
 get-object-graph/by-type)

(define (get-object-counts)
  null)

(define (get-object-graph _proc)
  (hasheq 'metadata (hasheq)
          'objects null
          'links (hasheq)))

(define (get-object-graph/by-type _type)
  (get-object-graph void))
