#lang racket/base

(provide
 call-with-limited-output-string)

(define (call-with-limited-output-string cap proc)
  (define len 0)
  (define out-bs (make-bytes cap))
  (define res-bs
    (let/ec esc
      (define out
        (make-output-port
         "limited"
         always-evt
         (lambda (data start end _dont-block? _enable-breaks?)
           (define remaining
             (- cap len))
           (define want
             (- end start))
           (cond
             [(zero? remaining)
              (esc out-bs)]
             [(< want remaining)
              (bytes-copy! out-bs len data start end)
              (set! len (+ len want))
              want]
             [else
              (bytes-copy! out-bs len data start (+ start remaining))
              (set! len (+ len remaining))
              remaining]))
         void))
      (proc out)
      (subbytes out-bs 0 len)))
  (bytes->string/utf-8 res-bs #\�))
