#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/gui/easy
         "dot.rkt"
         "reference-graph.rkt")

(provide
 render-backreferences)

(define (render-backreferences what graph)
  (define root
    (render
     (window
      #:title (format "~a Backreferences" what)
      #:size '(800 600)
      (reference-graph
       graph
       (lambda (item event)
         (render-popup-menu
          root
          (popup-menu
           (menu-item
            "Save Graph..."
            (λ ()
              (write-graph-pdf
               (gui:put-file "Save Graph..." #f #f #f #f null '(("PDF" "*.pdf")))
               graph
               (hash-ref item 'id))))
           (menu-item
            "Copy..."
            (λ ()
              (send
               gui:the-clipboard
               set-clipboard-string
               (hash-ref item 'str)
               (send event get-time-stamp))))
           (menu-item-separator)
           (menu-item
            "Narrow..."
            (λ ()
              (define narrowed-graph
                (hash-set graph 'objects (list (hash-ref item 'id))))
              (render-backreferences what narrowed-graph)))
           (menu-item
            "Filter by hash..."
            (λ ()
              (define the-hash (hash-ref item 'hash))
              (define metadata (hash-ref graph 'metadata))
              (define filtered-objects
                (for*/list ([id (in-list (hash-ref graph 'objects))]
                            [ob (in-value (hash-ref metadata id))]
                            #:when (= (hash-ref ob 'hash) the-hash))
                  id))
              (define filtered-graph
                (hash-set graph 'objects filtered-objects))
              (render-backreferences what filtered-graph)))
           (menu-item
            "Sort by count..."
            (λ ()
              (define metadata (hash-ref graph 'metadata))
              (define-values (hashes-to-ids sorted-hashes)
                (for*/fold ([hashes (hasheqv)]
                            [counts (hasheqv)]
                            #:result (values hashes (map car (sort (hash->list counts) > #:key cdr))))
                           ([id (in-list (hash-ref graph 'objects))]
                            [ob (in-value (hash-ref metadata id))])
                  (define the-hash (hash-ref ob 'hash))
                  (values
                   (hash-update hashes the-hash (λ (ids) (cons id ids)) null)
                   (hash-update counts the-hash add1 0))))
              (define sorted-graph
                (hash-set graph 'objects (for*/list ([h (in-list sorted-hashes)]
                                                     [id (in-list (hash-ref hashes-to-ids h))])
                                           id)))
              (render-backreferences what sorted-graph))))
          (send event get-x)
          (send event get-y)))))))
  (void))
