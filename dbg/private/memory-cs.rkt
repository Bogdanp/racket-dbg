#lang racket/base

(require ffi/unsafe/atomic
         ffi/unsafe/vm
         racket/port)

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
                [str (in-value (->string (car c)))]
                [gen (in-list (cdr c))])
      (hash-update res str
                   (λ (cnt)
                     (cons
                      (+ (cadr gen) (car cnt))
                      (+ (cddr gen) (cdr cnt))))
                   (cons 0 0))))
  (sort (hash-map counts-by-str cons) #:key cddr >))

(define (get-object-graph proc)
  (define-values (_counts backrefs)
    (get-counts&backrefs))
  (define id-seq 0)
  (define ids (make-hasheq))
  (define (id ob)
    (hash-ref! ids ob (lambda ()
                        (begin0 id-seq
                          (set! id-seq (add1 id-seq))))))
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
  (define metadata (make-hasheqv))
  (define links (make-hasheqv))
  (define (track! ob)
    (define ob-id (id ob))
    (unless (hash-has-key? metadata ob-id)
      (hash-set! metadata ob-id (object-metadata ob ob-id))
      (for ([ref (in-list (hash-ref graph ob null))])
        (define ref-id (id ref))
        (hash-update! links ob-id
                      (λ (ob-ids)
                        (cons ref-id ob-ids))
                      null))
      (for ([ref (in-list (hash-ref graph ob null))])
        (track! ref))))
  (define objects
    (for/list ([ob (in-hash-keys graph)] #:when (proc ob))
      (track! ob)
      (id ob)))
  (hasheq
   'metadata metadata
   'objects objects
   'links links))

(define (get-object-graph/by-struct name)
  (get-object-graph (lambda (ob)
                      (and (record? ob)
                           (let ([rtd (record-rtd ob)])
                             (eq? name (record-type-name rtd)))))))

(define (->string ob)
  (call-with-output-string
   (lambda (out)
     (write ob out))))

(define (object-metadata ob ob-id)
  (hasheq
   'id ob-id
   'str (->string ob)
   'name (cond
           [(object-name ob) => ->string]
           [else #f])
   'type (object-type ob)))

(define (object-type ob)
  (cond
    [(boolean? ob) 'boolean]
    [(box? ob) 'box]
    [(bytes? ob) 'bytes]
    [(hash? ob) 'hash]
    [(list? ob) 'list]
    [(pair? ob) 'pair]
    [(port? ob) 'port]
    [(procedure? ob) 'procedure]
    [(record? ob) (record-type-name (record-rtd ob))]
    [(string? ob) 'string]
    [(vector? ob) 'vector]
    [else #f]))
