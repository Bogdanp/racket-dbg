#lang info

(define license 'BSD-3-Clause)
(define version "0.2")
(define collection "debugging")
(define deps '("base"
               "canvas-list"
               ["dbg" #:version "0.4"]
               ["gui-easy-lib" #:version "0.16.1"]
               "gui-lib"
               "pict-lib"
               "plot-gui-lib"
               "plot-lib"
               "profile-lib"))
(define build-deps '("rackunit-lib"))
(define raco-commands
  '(("dbg" (submod debugging/ui main) "run a remote debugger" #f)))
