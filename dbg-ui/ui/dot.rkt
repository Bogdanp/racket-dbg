#lang racket/base

(require racket/format
         racket/match
         racket/port
         racket/string
         racket/system
         "common.rkt")

(provide
 write-graph-pdf)

(define (write-graph-pdf target graph start)
  (define metadata (hash-ref graph 'metadata))
  (define links (hash-ref graph 'links))
  (define (~node id)
    (define meta (hash-ref metadata id))
    (string-replace
     (~a
      (~shortstr
       (or
        (hash-ref meta 'name)
        (hash-ref meta 'str)))
      " @ "
      (~addr id))
     "\""
     "\\\""))
  (define graph-str
    (with-output-to-string
      (lambda ()
        (printf "digraph {~n")
        (let loop ([id start])
          (for ([ref-id (in-list (hash-ref links id null))])
            (printf "\"~a\" -> \"~a\";~n" (~node id) (~node ref-id))
            (loop ref-id)))
        (printf "}")
        (void))))

  (define stderr (open-output-string))
  (match-define (list _in _out _pid _err ctl)
    (process*/ports
     (current-output-port)
     (open-input-string graph-str)
     stderr
     (find-executable-path "dot")
     "-Tpdf" "-o" target))
  (ctl 'wait)
  (unless (zero? (ctl 'exit-code))
    (error 'write-graph-pdf "error: ~a" (get-output-string stderr))))
