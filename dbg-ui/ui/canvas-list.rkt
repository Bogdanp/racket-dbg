#lang racket/base

(require (prefix-in cl: canvas-list)
         racket/class
         racket/gui/easy
         racket/gui/easy/operator
         racket/list)

(provide
 canvas-list)

(define canvas-list%
  (class* object% (view<%>)
    (init-field @items draw action context-action item-id item-height item-color alt-color selection-color hover-color)
    (super-new)

    (define items (obs-peek @items))

    (define/public (dependencies)
      (list @items))

    (define/public (create parent)
      (new cl:canvas-list%
           [parent parent]
           [items items]
           [paint-item-callback (λ (_self item state dc w h)
                                  (send dc set-smoothing 'smoothed)
                                  (draw item state dc w h))]
           [action-callback (λ (_self item event)
                              (action item event))]
           [context-action-callback (λ (_self item event)
                                      (context-action item event))]
           [item-height item-height]
           [item-color item-color]
           [alt-color alt-color]
           [selection-color selection-color]
           [hover-color hover-color]))

    (define/public (update v what val)
      (case/dep what
        [@items
         ;; Update the items while trying to preserve the current
         ;; selection, if any.
         (unless (equal? items val)
           (define item (send v get-selected-item))
           (set! items val)
           (send v set-items val)
           (define item-ids (map item-id items))
           (define index (and item (index-of item-ids (item-id item) eq?)))
           (when index
             (send v select-index index)))]))

    (define/public (destroy _v)
      (void))))

(define (canvas-list @items draw
                     #:action [action void]
                     #:context-action [context-action void]
                     #:item-id [item-id values]
                     #:item-height [item-height 20]
                     #:item-color [item-color #f]
                     #:alt-color [alt-color #f]
                     #:selection-color [selection-color #f]
                     #:hover-color [hover-color #f])
  (new canvas-list%
       [@items (@ @items)]
       [draw draw]
       [action action]
       [context-action context-action]
       [item-id item-id]
       [item-height item-height]
       [item-color item-color]
       [alt-color alt-color]
       [selection-color selection-color]
       [hover-color hover-color]))
