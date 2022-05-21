#lang racket/base

(require (prefix-in p: pict)
         racket/format
         racket/gui/easy
         "common.rkt"
         "resource.rkt"
         "tree-list.rkt")

(provide
 reference-graph)

(define selected-color (color "blue"))
(define black (color "black"))
(define white (color "white"))

;; 24x24
(define chevron-down-icon (icon 'chevron-down))
(define chevron-forward-icon (icon 'chevron-forward))

(define (state->colors s)
  (case s
    [(selected)
     (values selected-color white)]
    [else
     (values white black)]))

(define (item-pict item state depth open? w h)
  (define-values (bg-color fg-color)
    (state->colors state))
  (define i-pict
    (p:hc-append
     5
     (p:colorize
      (if open?
          (p:bitmap chevron-down-icon)
          (p:bitmap chevron-forward-icon))
      fg-color)
     (p:colorize
      (p:text
       (format "~a @ ~a [#~a]"
               (~shortstr (hash-ref item 'str))
               (~addr (hash-ref item 'id))
               (~hash (hash-ref item 'hash)))
       mono-font/12)
      fg-color)))
  (p:lt-superimpose
   (p:colorize (p:filled-rectangle w h) bg-color)
   (p:inset i-pict (+ 5 (* depth 5)) 5)))

(define (reference-graph graph [context-action void])
  (define metadata (hash-ref graph 'metadata))
  (define objects (hash-ref graph 'objects))
  (define links (hash-ref graph 'links))
  (define tree
    (for/list ([id (in-list objects)])
      (let loop ([id id])
        (define meta (hash-ref metadata id))
        (define refs (hash-ref links id null))
        (list meta (for/list ([ref (in-list refs)])
                     (loop ref))))))
  (tree-list
   tree
   #:item-height 24
   (lambda (item state depth open? dc w h)
     (p:draw-pict (item-pict item state depth open? w h) dc 0 0))
   #:context-action context-action))

(module+ main
  (require racket/runtime-path)
  (define-runtime-path graph.rktd "testdata/graph.rktd")
  (define graph (call-with-input-file graph.rktd read))
  (render
   (window
    #:title "Test"
    #:size '(800 600)
    (reference-graph graph))))
