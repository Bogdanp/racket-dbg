#lang racket/base

(require racket/match)

(provide
 display-thread-stacks)

(define (display-thread-stacks
         [cust (current-custodian)]
         [root (current-custodian)]
         [out (current-output-port)])
  (let loop ([v cust])
    (cond
      [(thread? v)
       (display-stack v out)]
      [(custodian? v)
       (for-each loop (custodian-managed-list v root))]
      [else
       (void)])))

(define (display-stack thd out)
  (define ctxt
    (continuation-mark-set->context
     (continuation-marks thd)))
  (fprintf out "in thread ~a:~n" (object-name thd))
  (define-values (last-frame last-frame-reps)
    (for/fold ([prev #f]
               [prev-reps 0])
              ([frame (in-list ctxt)])
      (cond
        [(equal? prev frame)
         (values prev (add1 prev-reps))]
        [else
         (when prev
           (display-frame prev prev-reps out))
         (values frame 1)])))
  (if last-frame
      (display-frame last-frame last-frame-reps out)
      (fprintf out "  <unknown>~n")))

(define (display-frame frame reps out)
  (match-define (cons proc loc) frame)
  (display-srcloc loc out)
  (define reps-str
    (cond
      [(= reps 1) ""]
      [else (format " (~a times)" reps)]))
  (if proc
      (fprintf out " ~a~a~n" proc reps-str)
      (fprintf out " <unknown function>~a~n" reps-str)))

(define (display-srcloc loc out)
  (match-define (srcloc source line column position _) loc)
  (let* ([source (if (path? source)
                     (path->string source)
                     source)]
         [source (let ([len (string-length source)])
                   (if (len . > . 22)
                       (string-append "..." (substring source (len . - . 19)))
                       source))])
    (fprintf out "  ~a:" source))
  (if (and line column)
      (fprintf out "~a:~a:" line column)
      (fprintf out "~a:" position)))
