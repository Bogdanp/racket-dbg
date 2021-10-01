#lang racket/base

(require "ui/main.rkt")
(provide (all-from-out "ui/main.rkt"))

(module+ main
  (require debugging/client
           racket/cmdline
           racket/gui)

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

  (define conn
    (connect
     #:host host
     #:port port))

  (let/cc esc
    (uncaught-exception-handler
     (λ (e)
       (define message
         (call-with-output-string
           (λ (out)
             (parameterize ([current-error-port out])
               ((error-display-handler) (exn-message e) e)))))
       (message-box "Error" message #f '(stop ok))
       (esc)))

    (void (start-ui conn))))
