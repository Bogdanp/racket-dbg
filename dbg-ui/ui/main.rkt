#lang racket/base

(require debugging/client
         plot
         racket/class
         racket/format
         racket/gui/easy
         racket/gui/easy/operator
         racket/list
         racket/match)

;; state ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct memory-tick (ts amount)
  #:transparent)

(struct gc-tick (ts mode amount duration)
  #:transparent)

(struct state
  (history
   memory-use
   memory-use/max
   memory-use/time
   gc-duration/total
   gc-duration/max
   gcs/time)
  #:transparent)

(define (make-state)
  (state 100 0 0 null 0 0 null))

(define (set-memory-use s amt)
  (struct-copy state s
               [memory-use amt]
               [memory-use/max (max (state-memory-use/max s) amt)]))

(define (add-gc-tick s ts i)
  (define hist (state-history s))
  (define mode (gc-info-mode i))
  (define amt (gc-info-post-amount i))
  (define duration
    (- (gc-info-end-time i)
       (gc-info-start-time i)))
  (struct-copy
   state s
   [memory-use amt]
   [memory-use/max (max
                    (state-memory-use/max s)
                    (gc-info-pre-amount i))]
   [memory-use/time (keep-right
                     (append
                      (state-memory-use/time s)
                      `(,(memory-tick ts amt)))
                     hist)]
   [gc-duration/total (+ (state-gc-duration/total s) duration)]
   [gc-duration/max (max (state-gc-duration/max s) duration)]
   [gcs/time (keep-right
              (append
               (state-gcs/time s)
               `(,(gc-tick ts mode amt duration)))
              hist)]))

(define (start-async-handler @state evt)
  (thread
   (lambda ()
     (let loop ()
       (sync
        (handle-evt
         evt
         (λ (topic&data)
           (match topic&data
             [`(gc ,ts ,info)
              (@state . <~ . (λ (s)
                               (add-gc-tick s ts info)))
              (loop)]

             [_
              (loop)]))))))))

;; components ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((make-window-mixin c) %)
  (class %
    (super-new)
    (define/augment (on-close)
      (disconnect c))))

(define (labeled label v)
  (hpanel
   #:stretch '(#t #f)
   (hpanel
    #:min-size '(130 #f)
    #:stretch '(#f #t)
    (text label))
   v))

(define (info-tab c)
  (define info (get-info c))
  (vpanel
   #:stretch '(#t #t)
   #:alignment '(left top)
   (labeled "Operating system:" (text (~a (hash-ref info 'os*))))
   (labeled "Virtual machine:" (text (~a (hash-ref info 'vm))))
   (labeled "Architecture:" (text (~a (hash-ref info 'arch))))
   (labeled "Version:" (text (hash-ref info 'version)))))

(define (charts-tab @state action)
  (define/obs @have-gc-data?
    (@state . ~> . (compose1 not null? state-memory-use/time)))
  (define/obs @hist
    (state-history (obs-peek @state)))
  (vpanel
   (labeled "Memory use:" (text (@state . ~> . (compose1 ~MiB state-memory-use))))
   (labeled "Total GC time:" (text (@state . ~> . (compose1 ~ms state-gc-duration/total))))
   (labeled "Longest GC pause:" (text (@state . ~> . (compose1 ~ms state-gc-duration/max))))
   (vpanel
    (hpanel
     #:stretch '(#t #f)
     (labeled
      "Max history:"
      (input
       #:stretch '(#f #f)
       #:min-size '(240 #f)
       (@hist . ~> . number->string)
       (λ (event text)
         (case event
           [(return)
            (define hist (string->number text))
            (when hist
              (@hist . := . hist)
              (action `(commit-history ,hist)))])))))
    (cond-view
     [@have-gc-data?
      (hpanel
       (plot-canvas @state plot-memory-usage)
       (plot-canvas @state plot-gc-pauses))]

     [else
      (text "No GC data yet.")]))))

;; TODO: Refresh periodically.
(define (custodians-tab c)
  (define (->entries counts)
    (list->vector
     (sort
      (for/list ([(k v) (in-hash counts)])
        (cons k v))
      #:key cdr >)))

  (define/obs @counts
    (get-managed-item-counts c))

  (vpanel
   (button
    "Reload"
    (λ ()
      (@counts . := . (get-managed-item-counts c))))
   (table
    '("Kind" "Count")
    (@counts . ~> . ->entries)
    #:entry->row (λ (entry)
                   (vector
                    (~a (car entry))
                    (~a (cdr entry)))))))

(define (start-ui c)
  (define/obs @tab 'info)
  (define/obs @state
    (set-memory-use
     (make-state)
     (get-memory-use c)))
  (define/obs @state/throttled
    (obs-throttle
     #:duration 250
     @state))
  (start-async-handler @state (async-evt c))
  (subscribe c 'gc)
  (render
   (window
    #:title "Remote Debugger"
    #:size '(600 400)
    #:mixin (make-window-mixin c)
    (let ([the-tabs '(info charts custodians)])
      (tabs
       (map (compose1 string-titlecase symbol->string) the-tabs)
       (λ (event _choices index)
         (case event
           [(select)
            (@tab . := . (list-ref the-tabs index))]))
       (case-view
        @tab
        [(info)
         (info-tab c)]

        [(charts)
         (charts-tab
          @state/throttled
          (match-lambda
            [`(commit-history ,hist)
             (@state . <~ . (λ (s)
                              (struct-copy state s [history hist])))]))]

        [(custodians)
         (custodians-tab c)]

        [else
         (hpanel)]))))))

(define (plot-memory-usage s w h)
  (parameterize ([plot-title "Memory Use"]
                 [plot-x-label "Time"]
                 [plot-y-label "MiB"]
                 [plot-x-ticks (date-ticks)]
                 [plot-pen-color-map 'tab20c])
    (define max-memory (->MiB (state-memory-use/max s)))
    (define memory-use
      (for/list ([t (in-list (state-memory-use/time s))])
        `(,(memory-tick-ts t)
          ,(->MiB (memory-tick-amount t)))))
    (define major-gcs
      (for/list ([t (in-list (state-gcs/time s))]
                 #:when (eq? 'major (gc-tick-mode t)))
        `(,(gc-tick-ts t)
          ,(->MiB (gc-tick-amount t)))))

    (plot-snip
     #:width w
     #:height h
     #:y-min 0
     #:y-max (* 1.10 max-memory)
     (list
      (hrule
       #:label "Max Memory"
       #:style 'long-dash
       max-memory)
      (area
       #:label "Memory"
       #:color 4
       #:line1-color 4
       #:line1-style 'transparent
       #:line2-color 4
       memory-use)
      (points
       #:label "Major GC"
       #:sym 'times
       #:color 4
       #:size 12
       major-gcs)))))

(define (plot-gc-pauses s w h)
  (parameterize ([plot-title "GC Pauses"]
                 [plot-x-label "Time"]
                 [plot-y-label "Duration (ms)"]
                 [plot-x-ticks (date-ticks)]
                 [plot-pen-color-map 'tab20c])
    (define minor-gcs
      (for/list ([t (in-list (state-gcs/time s))]
                 #:when (eq? 'minor (gc-tick-mode t)))
        `(,(gc-tick-ts t)
          ,(gc-tick-duration t))))
    (define major-gcs
      (for/list ([t (in-list (state-gcs/time s))]
                 #:when (eq? 'major (gc-tick-mode t)))
        `(,(gc-tick-ts t)
          ,(gc-tick-duration t))))
    (plot-snip
     #:width w
     #:height h
     #:y-min 0
     #:y-max (* 1.10
                (max
                 (if (null? minor-gcs) 0 (apply max (map cadr minor-gcs)))
                 (if (null? major-gcs) 0 (apply max (map cadr major-gcs)))))
     (list
      (points #:label "Major GC" #:color 4 major-gcs)
      (points #:label "Minor GC" #:color 1 minor-gcs)))))

(define (plot-canvas @data make-plot-snip)
  (canvas
   @data
   (λ (dc data)
     (define-values (w h)
       (send dc get-size))
     (define snip
       (make-plot-snip data w h))
     (define bmp
       (send snip get-bitmap))
     (send dc draw-bitmap bmp 0 0))))

(define area
  (make-keyword-procedure
   (lambda (kws kw-args vs . args)
     (keyword-apply
      lines-interval
      kws kw-args
      (for/list ([t (in-list vs)])
        `(,(car t) 0))
      vs
      args))))


;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (keep-right xs n)
  (if (> (length xs) n)
      (take-right xs n)
      xs))

(define (->MiB v)
  (/ v 1024 1024))

(define (~MiB v)
  (format "~aMiB" (~r #:precision '(= 2) (/ v 1024 1024))))

(define (~ms v)
  (format "~a ms" v))


;; main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ main
  (require racket/cmdline)

  (define-values (host port)
    (let ([host "127.0.0.1"]
          [port 9011])
      (command-line
       #:once-each
       [("--host" "-H") the-host "the host to connect to (default: 127.0.0.1)"
                        (set! host the-host)]
       [("--port" "-P") the-port "the port to connect to (default: 9011)"
                        (let ([port-number (string->number the-port)])
                          (unless port-number
                            (eprintf "error: ~a is not a valid port number~n" the-port))
                          (set! port port-number))]
       #:args []
       (values host port))))

  (void
   (start-ui
    (connect
     #:host host
     #:port port))))
