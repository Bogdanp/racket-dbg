#lang racket/base

(require profile/analyzer
         racket/format
         racket/port
         (prefix-in tm: "tree-map.rkt"))

(provide
 profile-node->tree-map-tree
 ~profile-node)

(define (profile-node->tree-map-tree n)
  (define children
    (sort
     (for*/list ([e (in-list (node-callees n))]
                 [n (in-value (edge-callee e))])
       (tm:node
        n
        (edge-caller-time e)
        null))
     #:key tm:node-size >))
  (tm:node
   n
   (apply + (map tm:node-size children))
   children))

(define (~profile-node n [maxlen 50])
  (define src
    (cond
      [(node-src n)
       => (λ (loc)
            (call-with-output-string
             (λ (out)
               (fprintf out
                        "~a:~a:~a"
                        (srcloc-source loc)
                        (srcloc-line loc)
                        (srcloc-column loc)))))]
      [else #f]))
  (define name
    (cond
      [(node-id n)
       => (λ (id)
            (format "~a in ~a" src id))]
      [(node-src n) src]
      [else "???"]))
  (define len
    (string-length name))
  (if (> len maxlen)
      (~a "..." (substring name (- len (- maxlen 3))))
      name))
