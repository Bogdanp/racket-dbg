#lang info

(define version "0.1")
(define collection "debugging")
(define deps '("base"
               "dbg"
               "gui-easy-lib"
               "gui-lib"
               "plot-lib"
               "plot-gui-lib"
               "profile-lib"))
(define build-deps '("rackunit-lib"))
(define raco-commands
  '(("dbg" (submod debugging/ui main) "run a remote debugger" #f)))
