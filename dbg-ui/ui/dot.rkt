#lang racket/base

(require racket/match
         racket/system)

(provide
 write-graph-pdf)

(define (write-graph-pdf target graph)
  (define stderr (open-output-string))
  (match-define (list _in _out _pid _err ctl)
    (process*/ports
     (current-output-port)
     (open-input-string graph)
     stderr
     (find-executable-path "dot")
     "-Tpdf" "-o" target))
  (ctl 'wait)
  (unless (zero? (ctl 'exit-code))
    (error 'write-graph-pdf "error: ~a" (get-output-string stderr))))
