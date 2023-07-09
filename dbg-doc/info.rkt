#lang info

(define license 'BSD-3-Clause)
(define collection "debugging")
(define scribblings '(("dbg-manual.scrbl" () (tool))))
(define deps '("base"))
(define build-deps '("dbg"
                     "racket-doc"
                     "scribble-lib"))
