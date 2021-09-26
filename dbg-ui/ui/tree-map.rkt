#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/gui/easy
         racket/gui/easy/operator)

;; node ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (struct-out node))

(struct node (name size children)
  #:transparent)

(define (node-size/scaled n s)
  (inexact->exact (floor (* (node-size n) s))))


;; tree-map% ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 tree-map)

(define pen-color
  (send gui:the-color-database find-color "black"))
(define brush-color
  (make-object gui:color% 255 0 0 0.025))

(define tree-map-canvas%
  (class gui:canvas%
    (init-field tree scale)
    (inherit get-client-size get-virtual-size  get-view-start
             init-auto-scrollbars scroll
             refresh-now)
    (field [mouse-x 0]
           [mouse-y 0])
    (super-new [style '(hscroll vscroll)]
               [paint-callback (λ (_self dc)
                                 (send dc set-pen pen-color 1 'solid)
                                 (send dc set-brush brush-color 'solid)
                                 (define-values (vx vy)
                                   (get-view-start))
                                 (draw-node tree dc 0 0 (+ vx mouse-x) (+ vy mouse-y) scale))])

    (define size (node-size/scaled tree scale))
    (init-auto-scrollbars size size 0 0)

    (define/override (on-char e)
      (case (send e get-key-code)
        [(wheel-up)   (change-scale 1.05)]
        [(wheel-down) (change-scale (/ 1 1.05))]))

    (define pending-draw #f)
    (define (schedule-draw)
      (unless pending-draw
        (define deadline (alarm-evt (+ (current-inexact-milliseconds) 16)))
        (set! pending-draw
              (thread
               (λ ()
                 (sync deadline)
                 (refresh-now)
                 (set! pending-draw #f))))))

    (define drag-events null)
    (define (push-drag-event e)
      (set! drag-events (cons e drag-events)))
    (define pending-drag-flush #f)
    (define (schedule-drag-flush)
      (unless pending-drag-flush
        (define deadline (alarm-evt (+ (current-inexact-milliseconds) 16)))
        (set! pending-drag-flush
              (thread
               (λ ()
                 (sync deadline)
                 (define-values (dx dy)
                   (let ([es (reverse drag-events)])
                     (for/fold ([dx 0]
                                [dy 0])
                               ([e0 (in-list es)]
                                [e1 (in-list (cdr es))])
                       (values
                        (+ dx (- (send e0 get-x)
                                 (send e1 get-x)))
                        (+ dy (- (send e0 get-y)
                                 (send e1 get-y)))))))
                 (define-values (x y)
                   (get-view-start))
                 (define-values (cw ch)
                   (get-client-size))
                 (define-values (vw vh)
                   (get-virtual-size))
                 (define-values (w h)
                   (values (max 0 (- vw cw))
                           (max 0 (- vh ch))))
                 (define new-x (min w (max 0 (+ x dx))))
                 (define new-y (min h (max 0 (+ y dy))))
                 (scroll
                  (if (zero? w) 0 (/ new-x w))
                  (if (zero? h) 0 (/ new-y h)))
                 (set! drag-events null)
                 (set! pending-drag-flush #f))))))

    (define/override (on-event e)
      (set! mouse-x (send e get-x))
      (set! mouse-y (send e get-y))
      (case (send e get-event-type)
        [(left-down)
         (push-drag-event e)]
        [(motion)
         (schedule-draw)
         (when (send e dragging?)
           (push-drag-event e)
           (schedule-drag-flush))]))

    (define/public (set-tree t)
      (set! tree t)
      (change-scale 1.0))

    (define/public (set-scale s)
      (set! scale s))

    (define pending-scale #f)
    (define/public (change-scale m)
      (define next-scale (* scale m))
      (define next-size (node-size/scaled tree next-scale))
      (when (and (>  next-size 0)
                 (<= next-size 1000000))
        (set! scale next-scale)
        (unless pending-scale
          (define deadline (alarm-evt (+ (current-inexact-milliseconds) 16)))
          (set! pending-scale
                (thread
                 (λ ()
                   (sync deadline)
                   (define s (node-size/scaled tree scale))
                   (define-values (x y) (get-view-start))
                   (define-values (cw ch) (get-client-size))
                   (define-values (vw vh) (get-virtual-size))
                   (define-values (w h)
                     (values (max 0 (- vw cw))
                             (max 0 (- vh ch))))
                   (init-auto-scrollbars s s
                                         (if (zero? w) 0 (/ x w))
                                         (if (zero? h) 0 (/ y h)))
                   (set! pending-scale #f)))))))))

(define tree-map%
  (class* object% (view<%>)
    (init-field @tree @scale)
    (super-new)

    (define/public (dependencies)
      (list @tree @scale))

    (define/public (create parent)
      (new tree-map-canvas%
           [tree (obs-peek @tree)]
           [scale (obs-peek @scale)]
           [parent parent]))

    (define/public (update v what val)
      (case/dep what
        [@tree (send v set-tree val)]
        [@scale (send v set-scale val)]))

    (define/public (destroy _v)
      (void))))

(define (tree-map @tree [@scale 1/100])
  (new tree-map%
       [@tree (@ @tree)]
       [@scale (@ @scale)]))

(module+ test
  (render
   (window
    #:size '(400 400)
    (tree-map
     (node "a" 10000
           (list
            (node "b" 8000
                  (list
                   (node "d" 5000 null)
                   (node "e" 1000 null)))
            (node "c" 2000 null)))
     1/50))))


;; draw ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (draw-node n dc x y mouse-x mouse-y scale)
  (define (do-draw-node n x y)
    (define size (node-size/scaled n scale))
    (define children (node-children n))
    (define child-sizes (for/list ([c (in-list children)])
                           (node-size/scaled c scale)))
    (send dc draw-rectangle x y size size)

    (let loop ([cs children]
               [xs `(,x)]
               [ys `(,y)]
               [nx `(0)]
               [ny `(,size)])
      (unless (null? cs)
        (define c (car cs))
        (define s (node-size/scaled c scale))
        (when (>= s 10)
          (define cx (car xs))
          (define cy (car ys))
          (cond
            [(<= (+ cx s)
                 (+ x size))
             (do-draw-node c cx cy)
             (loop (cdr cs)
                   (cons (+ cx s) xs)
                   ys
                   (cons cx nx)
                   (cons (+ cy s) ny))]

            [else
             (define-values (y* nx* ny*)
               (let fit ([y  (car ny)]
                         [nx (cdr nx)]
                         [ny (cdr ny)])
                 (cond
                   [(<= (+ y s) (car ny))
                    (values y nx ny)]

                   [else
                    (fit (car ny)
                         (cdr nx)
                         (cdr ny))])))
             (define cx* (cadr nx))
             (define cy* y*)
             (do-draw-node c cx* cy*)
             (loop (cdr cs)
                   (cons (+ cx* s) xs)
                   (cons cy* ys)
                   (cons cx* nx*)
                   (cons (+ cy* s) ny*))]))))

    (when (and (>= mouse-x x)
               (>= mouse-y y)
               (<= mouse-x (+ size x))
               (<= mouse-y (+ size y))
               (not (ormap (λ (s) (< (- size s) 30)) child-sizes)))
      (send dc draw-text (node-name n) (+ x 5) (+ y size -18))))
  (do-draw-node n 0 0))
