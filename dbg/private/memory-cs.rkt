#lang racket/base

(require ffi/unsafe/atomic
         ffi/unsafe/vm
         racket/port
         racket/string)

(provide
 get-object-counts
 get-object-graph
 get-object-graph/by-struct)

(define enable-object-counts (vm-primitive 'enable-object-counts))
(define enable-object-backreferences (vm-primitive 'enable-object-backreferences))
(define object-counts (vm-primitive 'object-counts))
(define object-backreferences (vm-primitive 'object-backreferences))
(define record? (vm-primitive 'record?))
(define record-rtd (vm-primitive 'record-rtd))
(define record-type-name (vm-primitive 'record-type-name))

(define (call-with-counts-enabled proc)
  (call-as-atomic
   (lambda ()
     (enable-object-counts #t)
     (enable-object-backreferences #t)
     (collect-garbage)
     (begin0 (proc)
       (enable-object-counts #f)
       (enable-object-backreferences #f)))))

(define (get-counts&backrefs)
  (call-with-counts-enabled
   (lambda ()
     (values
      (object-counts)
      (object-backreferences)))))

(define (get-object-counts)
  (define-values (counts _backrefs)
    (get-counts&backrefs))
  (define counts-by-str
    (for*/fold ([res (hash)])
               ([c (in-list counts)]
                [str (in-value (object->string (car c)))]
                [gen (in-list (cdr c))])
      (hash-update res str
                   (λ (cnt)
                     (cons
                      (+ (cadr gen) (car cnt))
                      (+ (cddr gen) (cdr cnt))))
                   (cons 0 0))))
  (sort (hash-map counts-by-str cons) #:key cddr >))

(define (get-object-graph proc)
  (define backrefs
    (call-with-counts-enabled
     (lambda ()
       (object-backreferences))))
  (define graph
    (make-hasheq))
  (for* ([gen-backrefs (in-list backrefs)]
         [backref (in-list gen-backrefs)])
    (define ob (car backref))
    (define ref (cdr backref))
    (hash-update!
     graph ob
     (lambda (refs)
       (cons ref refs))
     null))
  (define id-seq 0)
  (define ids (make-hasheq))
  (define (id ob)
    (hash-ref! ids ob (lambda ()
                        (begin0 id-seq
                          (set! id-seq (add1 id-seq))))))
  (define (dotify ob)
    (string-replace
     (call-with-output-string
      (lambda (out)
        (fprintf out "~.s" ob)
        (display "@" out)
        (display (id ob) out)))
     "\"" "\\\""))
  (define seen (make-hash))
  (with-output-to-string
    (lambda ()
      (printf "digraph {~n")
      (for ([(ob refs) (in-hash graph)] #:when (proc ob))
        (for ([ref (in-list refs)])
          (printf "\"~a\" -> \"~a\";~n" (dotify ob) (dotify ref))
          (let loop ([ref ref])
            (define subrefs (hash-ref graph ref null))
            (unless (null? subrefs)
              (for ([subref (in-list subrefs)])
                (define k (cons ref subref))
                (unless (hash-has-key? seen k)
                  (printf "\"~a\" -> \"~a\";~n" (dotify ref) (dotify subref))
                  (hash-set! seen k #t)))
              (for-each loop subrefs)))))
      (printf "}"))))

(define (get-object-graph/by-struct name)
  (get-object-graph (lambda (ob)
                      (and (record? ob)
                           (let ([rtd (record-rtd ob)])
                             (eq? name (record-type-name rtd)))))))

(define (object->string o)
  (call-with-output-string
   (lambda (out)
     (write o out))))
