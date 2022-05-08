#lang racket/base

(require racket/format)

(provide
 ~addr
 ~shortstr)

(define (~addr n)
  (~a "0x" (~r
            #:base 16
            #:min-width 8
            #:pad-string "0"
            n)))

(define (~shortstr s)
  (if (> (string-length s) 70)
      (~a (substring s 0 69) "…")
      s))
