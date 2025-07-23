#lang racket/base

(require "reference-graph.rkt")

(provide
 memory-hogs)

;; Too lazy to implement a bespoke graph view for hogs for now, so just
;; reuse the reference-graph implementation we have already. Translate
;; the hogs data structure to a reference-graph graph and display it.
(define (memory-hogs hogs)
  (define metadata
    (make-hasheqv))
  (define links
    (make-hasheqv))
  (define (track! v)
    (define id
      (hash-ref v 'id))
    (define meta
      (hasheq
       'id id
       'str (hash-ref v 'name)
       'len (hash-ref v 'total 0)))
    (hash-set! metadata id meta)
    (for ([child (in-list (hash-ref v 'children null))])
      (define child-id (hash-ref child 'id))
      (hash-update! links id (λ (ids) (cons child-id ids)) null)
      (track! child)))
  (for-each track! hogs)
  (define objects
    (for/list ([id (in-hash-keys metadata)]
               #:unless (ormap (λ (ids) (memv id ids))
                               (hash-values links)))
      id))
  (define (len id)
    (hash-ref (hash-ref metadata id) 'len 0))
  (define sorted-objects
    (sort objects > #:key len))
  (define sorted-links
    (for/hasheqv ([(id links) (in-hash links)])
      (values id (sort links > #:key len))))
  (reference-graph
   (hasheq
    'objects sorted-objects
    'metadata metadata
    'links sorted-links)))

(module+ main
  (require racket/gui/easy
           racket/runtime-path)
  (define-runtime-path hogs.rktd "testdata/hogs.rktd")
  (define hogs (call-with-input-file hogs.rktd read))
  (render
   (window
    #:title "Test"
    #:size '(800 600)
    (memory-hogs hogs))))
