#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         "hacks.rkt")

(provide
 make-mix-window-close
 mix-context-event)

(define (make-mix-window-close)
  (define close! void)
  (values
   (lambda ()
     (close!))
   (lambda (%)
     (class %
       (inherit show)
       (super-new)
       (set! close! (λ () (show #f)))))))

(define ((mix-context-event proc) %)
  (class %
    (super-new)
    (define/override (on-subwindow-event receiver event)
      (case (send event get-event-type)
        [(right-down)
         (define maybe-item-index
           (cond
             [(is-a? receiver gui:list-box%)
              (define item-height
                (case (system-type 'os)
                  ;; On macOS, the table can display a partial item at the end, depending
                  ;; on the height, but number-of-visible-items returns the number of
                  ;; items that are fully visible, so we have to hard code the row height.
                  [(macosx) 18]
                  [else
                   (quotient
                    (send receiver get-height)
                    (case (system-type 'os)
                      [(unix) (send receiver number-of-visible-items)]
                      [else (add1 (send receiver number-of-visible-items))]))]))
              (define y-pos
                (let ([y (send event get-y)])
                  (case (system-type 'os)
                    [(unix windows) ;; does not include scroll offset
                     (+ y (* item-height (send receiver get-first-visible-item)))]
                    [else y])))
              (define item-index
                (let* ([index (quotient y-pos item-height)]
                       [index (case (system-type 'os)
                                [(macosx unix) index] ;; the first non-header row has y=0
                                [else (sub1 index)])])
                  (and (< index (send receiver get-number)) index)))
              (begin0 item-index
                (when item-index
                  (send receiver select item-index #t)))]
             [else #f]))
         (make-mouse-event-positions-absolute receiver event)
         (begin0 #t
           (proc event maybe-item-index))]
        [else #f]))))
