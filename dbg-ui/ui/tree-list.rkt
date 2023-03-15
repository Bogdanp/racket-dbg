#lang racket/base

(require racket/gui/easy/operator
         racket/list
         "canvas-list.rkt")

(provide
 tree-list)

(define max-scrollbar-height
  (* 1 1000 1000))

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
  (reverse
   (let loop ([children (tree-children t)])
     (for/fold ([items null])
               ([e (in-list children)])
       (cond
         [(container? e)
          (if (container-open? e)
              (append (loop (container-children e)) (cons e items))
              (cons e items))]
         [else
          (cons e items)])))))

(define ((make-tree->items limit) t)
  (define items (tree->items t))
  (define n-items (length items))
  (if (> n-items limit)
      (begin0 (take items limit)
        (eprintf "Warning: truncated litem list from ~a to ~a~n" n-items limit))
      items))

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
  ;; HACK: The GUI has a contract on the maximum height of scrollbars
  ;; and canvas-list doesn't protect against it, so we have to
  ;; truncate our list when it contains too many items.
  (define max-items (quotient max-scrollbar-height item-height))
  (define @tree (@ (list->tree t)))
  (define @items (@tree . ~> . (make-tree->items max-items)))
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

(module+ test
  (require rackunit)
  (define t
    (list->tree
     (list
      (list "a" (list "b" (list "c" (list "d"))))
      (list "b" (list "d")))))
  (check-equal?
   (tree->items t)
   (list
    (container "a" 0 #f (list (entry "b" 1) (container "c" 1 #f (list (entry "d" 2)))))
    (container "b" 0 #f (list (entry "d" 1)))))
  (check-equal?
   (tree->items (tree-toggle t (list-ref (tree-children t) 0)))
   (list
    (container "a" 0 #t (list (entry "b" 1) (container "c" 1 #f (list (entry "d" 2)))))
    (entry "b" 1)
    (container "c" 1 #f (list (entry "d" 2)))
    (container "b" 0 #f (list (entry "d" 1)))))
  (check-equal?
   (tree->items
    (let* ([t (tree-toggle t (car (tree-children t)))]
           [t (tree-toggle t (cadr (container-children (car (tree-children t)))))])
      t))
   (list
    (container "a" 0 #t (list (entry "b" 1) (container "c" 1 #t (list (entry "d" 2)))))
    (entry "b" 1)
    (container "c" 1 #t (list (entry "d" 2)))
    (entry "d" 2)
    (container "b" 0 #f (list (entry "d" 1)))))
  (check-equal?
   (tree->items (tree-toggle t (list-ref (tree-children t) 1)))
   (list
    (container "a" 0 #f (list (entry "b" 1) (container "c" 1 #f (list (entry "d" 2)))))
    (container "b" 0 #t (list (entry "d" 1)))
    (entry "d" 1)))
  (check-equal?
   (tree->items
    (let* ([t (tree-toggle t (car (tree-children t)))]
           [t (tree-toggle t (cadr (tree-children t)))]
           [t (tree-toggle t (cadr (container-children (car (tree-children t)))))])
      t))
   (list
    (container "a" 0 #t (list (entry "b" 1) (container "c" 1 #t (list (entry "d" 2)))))
    (entry "b" 1)
    (container "c" 1 #t (list (entry "d" 2)))
    (entry "d" 2)
    (container "b" 0 #t (list (entry "d" 1)))
    (entry "d" 1))))
