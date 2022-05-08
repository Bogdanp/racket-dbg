#lang racket/base

(require racket/format)

(provide
 ~shortstr)

(define (~shortstr s)
  (if (> (string-length s) 70)
      (~a (substring s 0 69) "…")
      s))
