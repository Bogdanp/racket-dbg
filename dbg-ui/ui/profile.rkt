#lang racket/base

(require profile/analyzer
         racket/format
         racket/port
         (prefix-in tm: "tree-map.rkt"))

(provide
 profile-node->tree-map-tree
 ~profile-node)

(define (profile-node->tree-map-tree prof n)
  (define seen (make-hasheq))
  (hash-set! seen n #t)
  (hash-set! seen (profile-*-node prof) #t)
  (define children
    (let loop ([edges (node-callees n)])
      (sort
       (for*/list ([e (in-list edges)]
                   [n (in-value (edge-callee e))]
                   #:unless (hash-has-key? seen n))
         (hash-set! seen n #t)
         (define children
           (loop (node-callees n)))
         (tm:node
          (~profile-node n)
          (apply + (node-self n) (map tm:node-size children))
          children))
       #:key tm:node-size >)))
  (tm:node
   (~profile-node n)
   (apply + (node-self n) (map tm:node-size children))
   children))

(define (~profile-node n [maxlen 50])
  (define name
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
      [(node-id n) => ~a]
      [else "???"]))
  (define len
    (string-length name))
  (if (> len maxlen)
      (~a "..." (substring name (- len (- maxlen 3))))
      name))
