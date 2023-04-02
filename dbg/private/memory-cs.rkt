#lang racket/base

(require ffi/unsafe/atomic
         ffi/unsafe/vm
         racket/port)

(provide
 get-object-counts
 get-object-graph
 get-object-graph/by-type)

(define enable-object-counts (vm-primitive 'enable-object-counts))
(define enable-object-backreferences (vm-primitive 'enable-object-backreferences))
(define object-counts (vm-primitive 'object-counts))
(define object-backreferences (vm-primitive 'object-backreferences))
(define record? (vm-primitive 'record?))
(define record-rtd (vm-primitive 'record-rtd))
(define record-type-name (vm-primitive 'record-type-name))
(define record-type-descriptor? (vm-primitive 'record-type-descriptor?))

(define (call-with-counts proc)
  (call-as-atomic
   (lambda ()
     (enable-object-counts #t)
     (enable-object-backreferences #t)
     (collect-garbage)
     (define counts (object-counts))
     (define backrefs (object-backreferences))
     (enable-object-counts #f)
     (enable-object-backreferences #f)
     (proc counts backrefs))))

(define (get-object-counts)
  (define counts-by-str
    (call-with-counts
     (lambda (counts _backrefs)
       (for*/fold ([res (hash)])
                  ([c (in-list counts)]
                   [str (in-value (type->string (car c)))]
                   [gen (in-list (cdr c))])
         (hash-update res str
                      (λ (cnt)
                        (cons
                         (+ (cadr gen) (car cnt))
                         (+ (cddr gen) (cdr cnt))))
                      (cons 0 0))))))
  (sort (hash-map counts-by-str cons) #:key cddr >))

(define (get-object-graph proc)
  (define metadata (make-hasheqv))
  (define links (make-hasheqv))
  (define objects
    (call-with-counts
     (lambda (_counts backrefs)
       (define id-seq 0)
       (define ids (make-hasheq))
       (define (id ob)
         (hash-ref! ids ob (lambda ()
                             (begin0 id-seq
                               (set! id-seq (add1 id-seq))))))


       (define graph (make-hasheq))
       (for* ([gen-backrefs (in-list backrefs)]
              [backref (in-list gen-backrefs)])
         (define ob (car backref))
         (define ref (cdr backref))
         (hash-update!
          graph ob
          (lambda (refs)
            (cons ref refs))
          null))

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

       (for/list ([ob (in-hash-keys graph)] #:when (proc ob))
         (track! ob)
         (id ob)))))

  (hasheq
   'metadata metadata
   'objects objects
   'links links))

(define (get-object-graph/by-type type)
  (get-object-graph
   (case type
     [(bignum) (vm-primitive 'bignum?)]
     [(box) (vm-primitive 'box?)]
     [(bytevector) (vm-primitive 'bytevector?)]
     [(continuation) (vm-primitive 'continuation?)]
     [(ephemeron) (vm-primitive 'ephemeron?)]
     [(fxvecotr) (vm-primitive 'fxvector?)]
     [(keyword) (vm-primitive 'keyword?)]
     [(pair) (vm-primitive 'pair?)]
     [(procedure) (vm-primitive 'procedure?)]
     [(stencil-vector) (vm-primitive 'stencil-vector?)]
     [(string) (vm-primitive 'string?)]
     [(symbol) (vm-primitive 'symbol?)]
     [(thread) thread?]
     [(vector) (vm-primitive 'vector?)]
     [(weakpair) (vm-primitive 'weak-pair?)]
     [(will-executor) (vm-primitive 'will-executor?)]
     [else (λ (ob)
             (and (record? ob)
                  (let ([rtd (record-rtd ob)])
                    (eq? (record-type-name rtd) type))))])))

(define (->string ob [max-length #f])
  (define str
    (if max-length
        (call-with-limited-output-string max-length (λ (out) (write ob out)))
        (call-with-output-string (λ (out) (write ob out)))))
  (cond
    [(and max-length (>= (string-length str) max-length))
     (string-set! str (sub1 max-length) #\…)
     (substring str 0 max-length)]
    [else
     str]))

(define (object-metadata ob ob-id)
  (hasheq
   'id ob-id
   'str (with-handlers ([exn:fail? (λ (_) "#<unknown>")])
          (->string ob 255))
   'hash (equal-hash-code ob)))

(define (call-with-limited-output-string cap proc)
  (define len 0)
  (define out-bs (make-bytes cap))
  (define res-bs
    (let/ec esc
      (define out
        (make-output-port
         "limited"
         always-evt
         (lambda (data start end _dont-block? _enable-breaks?)
           (define remaining
             (- cap len))
           (define want
             (- end start))
           (cond
             [(zero? remaining)
              (esc out-bs)]
             [(< want remaining)
              (bytes-copy! out-bs len data start end)
              (set! len (+ len want))
              want]
             [else
              (bytes-copy! out-bs len data start (+ start remaining))
              (set! len (+ len remaining))
              remaining]))
         void))
      (proc out)
      (subbytes out-bs 0 len)))
  (bytes->string/utf-8 res-bs #\�))

(define (type->string t)
  (symbol->string
   (if (record-type-descriptor? t)
       (record-type-name t)
       t)))
