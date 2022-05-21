#lang racket/base

(require racket/format
         racket/gui/easy)

(provide
 ~addr
 ~shortstr
 ~hash)

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

(define (~hash h)
  (~r #:base 16 #:min-width 16 #:pad-string "0" h))

(provide
 mono-font
 mono-font/12)

(define (get-mono-font size)
  (or
   (font* "Dank Mono, Operator Mono, PT Mono, SF Mono, Inconsolata, Consolas, Courier New, Courier" size)
   (font "Courier" size #:family 'modern)))

(define mono-font (get-mono-font 14))
(define mono-font/12 (get-mono-font 12))
