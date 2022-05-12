#lang racket/base

(require racket/format
         racket/gui/easy)

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

(provide
 mono-font)

(define mono-font
  (let ([size 14])
    (or
     (font* "Dank Mono, Operator Mono, PT Mono, SF Mono, Inconsolata, Consolas, Courier New, Courier" size)
     (font "Courier" size #:family 'modern))))
