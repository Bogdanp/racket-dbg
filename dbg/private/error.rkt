#lang racket/base

(provide
 (struct-out exn:fail:dbg)
 (struct-out exn:fail:dbg:client)
 (struct-out exn:fail:dbg:server))

(struct exn:fail:dbg exn:fail ())
(struct exn:fail:dbg:client exn:fail:dbg ())
(struct exn:fail:dbg:server exn:fail:dbg ())
