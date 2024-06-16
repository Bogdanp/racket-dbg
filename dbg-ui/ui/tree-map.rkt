#lang racket/base

(require racket/class
         racket/format
         (prefix-in gui: racket/gui)
         racket/gui/easy
         racket/gui/easy/operator
         racket/match
         racket/math)

;; node ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (struct-out node))

(struct node (data size children)
  #:transparent)


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
    (init-field tree scale
                [action void]
                [format-data ~a])
    (inherit get-size get-client-size get-virtual-size  get-view-start
             init-auto-scrollbars scroll
             refresh-now)
    (field [mouse-x 0]
           [mouse-y 0])
    (super-new [style '(hscroll vscroll)]
               [paint-callback (λ (_self dc)
                                 (define-values (w h)
                                   (get-virtual-size))

                                 (send dc set-pen pen-color 1 'solid)
                                 (send dc set-brush brush-color 'solid)
                                 (define rects (filter rect-big-enough? (compute-rects tree 0 0 w h)))
                                 (for ([r (in-list rects)])
                                   (match-define (rect _ x y w h) r)
                                   (send dc draw-rectangle x y w h))

                                 (define-values (vx vy) (get-view-start))
                                 (define-values (mx my)
                                   (values (+ vx mouse-x)
                                           (+ vy mouse-y)))
                                 (define best-match
                                   (for/last ([r (in-list rects)] #:when (rect-contains? r mx my))
                                     r))
                                 (when best-match
                                   (match-define (rect data x y _w h) best-match)
                                   (send dc draw-text
                                         (format-data data)
                                         (+ mx 0)
                                         (+ my 18))))])

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

    (define (find-best-match)
      (define-values (x y) (get-view-start))
      (define-values (w h) (get-virtual-size))
      (define rects (compute-rects tree 0 0 w h))
      (for/last ([r (in-list rects)]
                 #:when (rect-contains? r (+ x mouse-x) (+ y mouse-y)))
        r))

    (define last-click #f)
    (define (double-click? e)
      (define delta
        (and last-click
             (- (send e get-time-stamp)
                (send last-click get-time-stamp))))
      (and delta (< delta 300)))
    (define/override (on-event e)
      (set! mouse-x (send e get-x))
      (set! mouse-y (send e get-y))
      (case (send e get-event-type)
        [(left-down)
         (set! dragging? #t)
         (set! drag-velocity (v2 0 0))
         (push-drag-event e)]
        [(left-up)
         (when (double-click? e)
           (define bm (find-best-match))
           (action 'dclick (and bm (rect-data bm))))
         (set! last-click e)

         (set! dragging? #f)
         (push-drag-event e)
         (schedule-drag-flush)]
        [(right-up)
         (define bm (find-best-match))
         (action 'rclick (and bm (rect-data bm)))]
        [(motion)
         (schedule-draw)
         (when dragging?
           (push-drag-event e)
           (schedule-drag-flush))]))

    (define/public (set-tree t)
      (set! tree t)
      (set-scale 1)
      (change-scale 1.0))

    (define/public (set-scale s)
      (set! scale s))

    (define pending-scale #f)
    (define/public (change-scale m)
      (define next-scale (* scale m))
      (define-values (w h)
        (get-client-size))
      (define next-w (exact-truncate (* w next-scale)))
      (define next-h (exact-truncate (* h next-scale)))
      (when (and (> next-w 0) (<= next-w 1000000)
                 (> next-h 0) (<= next-h 1000000))
        (set! scale next-scale)
        (with-scheduled-cb pending-scale
          (define-values (x y) (get-view-start))
          (define-values (cw ch) (get-client-size))
          (define-values (vw vh) (get-virtual-size))
          (define-values (w* h*)
            (values (max 0 (- vw cw))
                    (max 0 (- vh ch))))
          (init-auto-scrollbars next-w next-h
                                (if (zero? w*) 0 (/ x w*))
                                (if (zero? h*) 0 (/ y h*))))))))

(define tree-map%
  (class* object% (view<%>)
    (init-field @tree @scale action format-data)
    (super-new)

    (define/public (dependencies)
      (list @tree @scale))

    (define/public (create parent)
      (new tree-map-canvas%
           [tree (obs-peek @tree)]
           [scale (obs-peek @scale)]
           [action action]
           [format-data format-data]
           [parent parent]))

    (define/public (update v what val)
      (case/dep what
        [@tree (send v set-tree val)]
        [@scale (send v set-scale val)]))

    (define/public (destroy _v)
      (void))))

(define (tree-map @tree
                  #:scale [@scale 1]
                  #:action [action void]
                  #:data->label [data->label ~a])
  (new tree-map%
       [@tree (@ @tree)]
       [@scale (@ @scale)]
       [action action]
       [format-data data->label]))

(module+ main
  (render
   (window
    #:size '(400 400)
    (tree-map
     #:scale 1
     (node "a" 10000
           (list
            (node "b" 8000
                  (list
                   (node "d" 5000 null)
                   (node "e" 1000 null)
                   (node "f"  500 null)
                   (node "g"  100 null)))
            (node "c" 2000 null))))))
  (render
   (window
    #:size '(400 400)
    (tree-map
     #:scale 1
     (node "a" 10000
           (list
            (node "b" 4000
                  (list
                   (node "c" 2000 null)
                   (node "d" 1500 null)
                   (node "e"  250 null)
                   (node "f"  250 null))))))))
  (render
   (window
    #:size '(400 400)
    (tree-map
     #:scale 1
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
     #:action (λ (event data)
                (printf "event: ~a data: ~s~n" event data))))))


;; draw ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct rect (data x y w h)
  #:transparent)

(define (rect-area r)
  (* (rect-w r)
     (rect-h r)))

(define (rect-big-enough? r)
  (>= (rect-area r) 1000))

(define (rect-contains? r x y)
  (match-define (rect _ rx ry w h) r)
  (and (>= x rx)
       (>= y ry)
       (<= x (+ rx w))
       (<= y (+ ry h))))

(define (best-fit w h area)
  (unless (<= area (* w h))
    (raise-arguments-error 'best-fit "(<=/c area (* w h))" "w" w "h" h "area" area))
  (if (>= w h)
      (values (quotient area h) h)
      (values w (quotient area w))))

(module+ test
  (require rackunit)
  (define (best-fit* w h area)
    (call-with-values
     (λ () (best-fit w h area))
     list))
  (check-equal? (best-fit* 200 100  1000) `( 10 100))
  (check-equal? (best-fit* 200 200  1000) `(  5 200))
  (check-equal? (best-fit* 200 400 10000) `(200  50)))

(define (compute-rects n x y w h)
  (define x-end (+ x w))
  (define y-end (+ y h))
  (define size (max 1 (node-size n)))
  (cons
   (rect (node-data n) x y w h)
   (for/fold ([x x]
              [y y]
              [rs null]
              #:result rs)
             ([c (in-list (node-children n))])
     (define w (- x-end x))
     (define h (- y-end y))
     (define child-size (node-size c))
     (define child-area (truncate (* (/ child-size size) (* w h))))
     (define-values (child-w child-h)
       (best-fit w h child-area))
     (define child-rects
       (append rs (compute-rects c x y child-w child-h)))
     (if (>= (+ x child-w) x-end)
         (values x (+ y child-h) child-rects)
         (values (+ x child-w) y child-rects)))))

(module+ test
  (require rackunit)

  (check-equal?
   (compute-rects
    (node "a" 0
          (list
           (node "b" 0 null)
           (node "c" 0 null)))
    0 0 200 100)
   (list
    (rect "a" 0 0 200 100)
    (rect "b" 0 0 0   100)
    (rect "c" 0 0 0   100)))

  (check-equal?
   (compute-rects
    (node "a" 10000
          (list
           (node "b" 8000
                 (list
                  (node "d" 5000 null)
                  (node "e" 1000 null)
                  (node "f"  500 null)
                  (node "g"  100 null)))
           (node "c" 2000 null)))
    0 0 200 100)
   (list
    (rect "a"   0   0 200 100)
    (rect "b"   0   0 160 100)
    (rect "d"   0   0 100 100)
    (rect "e" 100   0  60  12)
    (rect "f" 100  12  60   5)
    (rect "g" 100  17  60   1)
    (rect "c" 160   0  40  20))))
