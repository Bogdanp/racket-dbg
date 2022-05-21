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
              (render-backreferences what filtered-graph))))
          (send event get-x)
          (send event get-y)))))))
  (void))
