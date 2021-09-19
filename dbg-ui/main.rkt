#lang racket/base

(require debugging/client
         plot
         racket/class
         racket/gui/easy
         racket/gui/easy/operator
         racket/match)

(define (run host port)
  (define @state (@ null))
  (define c
    (connect
     #:host host
     #:port port))
  (subscribe c 'gc)
  (thread
   (λ ()
     (let loop ()
       (sync
        (handle-evt
         (async-evt c)
         (λ (topic&data)
           (match topic&data
             [`(gc ,(gc-info _ _ _ _ amt _ _ _ _ end-time))
              (@state . <~ . (λ (s) (cons `(,end-time ,amt) s)))
              (loop)]

             [_
              (loop)])))))))
  (render
   (window
    #:title "Remote Debugger"
    #:size '(400 800)
    #:mixin (λ (%)
              (class %
                (super-new)
                (define/augment (on-close)
                  (disconnect c))))
    (hpanel
     (dyn-view
      @state
      (λ (data)
        (snip data (λ (data w h)
                     (plot-snip
                      #:width w
                      #:height h
                      (lines data))))))))))

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
  (run host port))
