#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/gui/easy
         racket/gui/easy/operator
         racket/match)

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

(struct v2 (x y)
  #:transparent)

(define (v2zero? v)
  (and (zero? (v2-x v))
       (zero? (v2-y v))))

(define (v2- a b)
  (v2 (- (v2-x a) (v2-x b))
      (- (v2-y a) (v2-y b))))

(define (v2+ a b)
  (v2 (+ (v2-x a) (v2-x b))
      (+ (v2-y a) (v2-y b))))

(define (v2* v n)
  (v2 (* (v2-x v) n)
      (* (v2-y v) n)))

(define (mouse-event->v2 e)
  (v2 (send e get-x)
      (send e get-y)))

(define pen-color
  (send gui:the-color-database find-color "black"))
(define brush-color
  (make-object gui:color% 255 0 0 0.05))

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

    (define-syntax-rule (with-scheduled-cb/no-reset id e0 e ...)
      (unless id
        (define deadline
          (alarm-evt (+ (current-inexact-milliseconds) 8)))
        (set! id (thread
                  (λ ()
                    (sync deadline)
                    (gui:queue-callback
                     (λ () e0 e ...)))))))

    (define-syntax-rule (with-scheduled-cb id e0 e ...)
      (with-scheduled-cb/no-reset id
        e0 e ...
        (set! id #f)))

    (define pending-draw #f)
    (define (schedule-draw)
      (with-scheduled-cb pending-draw
        (refresh-now)))

    (define (near-zero? x)
      (< (abs x) 0.01))

    (define dragging? #f)
    (define drag-last #f)
    (define drag-velocity (v2 0 0))
    (define (push-drag-event e)
      (cond
        [drag-last
         (define ev2 (mouse-event->v2 e))
         (define dt
           (- (send e get-time-stamp)
              (send drag-last get-time-stamp)))
         (define accel
           (round (if (zero? dt) 4 (max 1 (/ 32 dt)))))
         (set! drag-velocity
               (v2+ drag-velocity
                    (v2* (v2- (mouse-event->v2 drag-last) ev2) accel)))
         (set! drag-last e)]
        [else
         (set! drag-last e)]))
    (define pending-drag-flush #f)
    (define (schedule-drag-flush [step 0])
      (with-scheduled-cb/no-reset pending-drag-flush
        (define-values (dx dy)
          (match drag-velocity
            [(v2 dx dy)
             #:when dragging?
             (values dx dy)]

            [(v2 dx dy)
             (values (* dx (/ 0.9 (expt 1.10 step)))
                     (* dy (/ 0.9 (expt 1.10 step))))]))
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
        (define new-h (if (zero? w) 0 (/ new-x w)))
        (define new-v (if (zero? h) 0 (/ new-y h)))
        (scroll new-h new-v)
        (set! pending-drag-flush #f)
        (cond
          [dragging?
           (set! drag-velocity (v2 0 0))]
          [else
           (set! drag-velocity
                 (v2 (if (near-zero? dx) 0 dx)
                     (if (near-zero? dy) 0 dy)))
           (cond
             [(v2zero? drag-velocity) (set! drag-last #f)]
             [else (schedule-drag-flush (add1 step))])])))

    (define/override (on-event e)
      (set! mouse-x (send e get-x))
      (set! mouse-y (send e get-y))
      (case (send e get-event-type)
        [(left-down)
         (set! dragging? #t)
         (set! drag-velocity (v2 0 0))
         (push-drag-event e)]
        [(left-up)
         (set! dragging? #f)
         (push-drag-event e)
         (schedule-drag-flush)]
        [(motion)
         (schedule-draw)
         (when dragging?
           (push-drag-event e)
           (schedule-drag-flush))]))

    (define/public (set-tree t)
      (set! tree t)
      (set-scale 1/10)
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
        (with-scheduled-cb pending-scale
          (define s (node-size/scaled tree scale))
          (define-values (x y) (get-view-start))
          (define-values (cw ch) (get-client-size))
          (define-values (vw vh) (get-virtual-size))
          (define-values (w h)
            (values (max 0 (- vw cw))
                    (max 0 (- vh ch))))
          (init-auto-scrollbars s s
                                (if (zero? w) 0 (/ x w))
                                (if (zero? h) 0 (/ y h))))))))

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

(define (tree-map @tree [@scale 1/10])
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
                   (node "e" 1000 null)
                   (node "f"  500 null)
                   (node "g"  100 null)))
            (node "c" 2000 null)))
     1/50)))
  (render
   (window
    #:size '(400 400)
    (tree-map
     (node "a" 10000
           (list
            (node "b" 4000
                  (list
                   (node "c" 2000 null)
                   (node "d" 1500 null)
                   (node "e"  250 null)
                   (node "f"  250 null)))))
     1/50)))
  (render
   (window
    #:size '(400 400)
    (tree-map
     (node "a" 10000
           (list
            (node "b" 5000
                  (list
                   (node "c" 2500 null)
                   (node "d" 1250 null)
                   (node "e"  625 null)
                   (node "f"  313 null)
                   (node "g"  157 null)
                   (node "h"   79 null)))))
     1/50))))


;; draw ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (draw-node n dc x y mouse-x mouse-y scale)
  (define (do-draw-node n x y)
    (define size (node-size/scaled n scale))
    (define children (node-children n))
    (define child-sizes
      (for/list ([c (in-list children)])
        (node-size/scaled c scale)))

    (send dc draw-rectangle x y size size)

    (let loop ([cs children]
               [nx `(,x)]
               [ny `(,y)])
      (unless (null? cs)
        (define c (car cs))
        (define s (node-size/scaled c scale))
        (when (>= s 5)
          (define cx (car nx))
          (define cy (car ny))
          (do-draw-node c cx cy)
          (loop (cdr cs)
                (append (cdr nx) `(,(+ cx s) ,cx))
                (append (cdr ny) `(,cy ,(+ cy s)))))))

    (when (and (>= mouse-x x)
               (>= mouse-y y)
               (<= mouse-x (+ size x))
               (<= mouse-y (+ size y))
               (not (ormap (λ (s) (< (- size s) 30)) child-sizes)))
      (send dc draw-text (node-name n) (+ x 5) (+ y size -18))))
  (do-draw-node n 0 0))
