#lang racket/base

(require racket/gui/easy/operator
         "canvas-list.rkt")

(provide
 tree-list)

(struct tree (children) #:transparent)
(struct entry (data depth) #:transparent)
(struct container entry (open? children) #:transparent)

(define (container-toggle c)
  (struct-copy container c [open? (not (container-open? c))]))

(define (set-container-children c children)
  (struct-copy container c [children children]))

(define (list->tree es)
  (tree
   (let loop ([es es] [depth 0])
     (for/list ([e (in-list es)])
       (if (pair? e)
           (container (car e) depth #f (loop (cadr e) (add1 depth)))
           (entry e depth))))))

(define (tree->items t)
  (let loop ([children (tree-children t)])
    (for/fold ([items null])
              ([e (in-list children)])
      (cond
        [(container? e)
         (if (container-open? e)
             (append items (cons e (loop (container-children e))))
             (append items (list e)))]
        [else
         (append items (list e))]))))

(define (tree-toggle t it)
  (tree
   (let loop ([es (tree-children t)])
     (for/list ([e (in-list es)])
       (cond
         [(eq? e it) (container-toggle e)]
         [(container? e) (set-container-children e (loop (container-children e)))]
         [else e])))))

(define (tree-list t draw
                   #:action [action void]
                   #:context-action [context-action void]
                   #:item-height [item-height 30])
  (define @tree (@ (list->tree t)))
  (define @items (@tree . ~> . tree->items))
  (canvas-list
   @items
   #:item-id entry-data
   #:item-height item-height
   (λ (item state dc w h)
     (define open? (and (container? item) (container-open? item)))
     (draw (entry-data item) state (entry-depth item) open? dc w h))
   #:action (λ (item _event)
              (if (container? item)
                  (@tree . <~ . (λ (old-t) (tree-toggle old-t item)))
                  (action (entry-data item))))
   #:context-action (λ (item event)
                      (context-action (entry-data item) event))))
