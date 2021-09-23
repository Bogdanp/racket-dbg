#lang racket/base

(require "ui/main.rkt")
(provide (all-from-out "ui/main.rkt"))

(module+ main
  (require debugging/client
           racket/cmdline)

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
