#lang racket/base

(require ffi/unsafe/atomic
         ffi/unsafe/vm
         racket/port
         "port.rkt")

(provide
 get-object-counts
 get-object-counts-by-module
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
(define record-accessor (vm-primitive 'record-accessor))
(define bignum? (vm-primitive 'bignum?))
(define bytevector-length (vm-primitive 'bytevector-length))
(define bytevector? (vm-primitive 'bytevector?))
(define continuation (vm-primitive 'continuation?))
(define fxvector? (vm-primitive 'fxvector?))
(define weak-pair? (vm-primitive 'weak-pair?))

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

;; (-> (listof (cons/c TYPE (cons/c COUNT SIZE))))
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

;; (-> TYPE (listof (cons/c MODULE COUNT)))
(define (get-object-counts-by-module type)
  (define ok? (get-predicate type))
  (define counts (make-hash))
  (call-with-counts
   (lambda (_counts backrefs)
     (define graph (build-graph backrefs))
     (for ([ob (in-hash-keys graph)])
       (when (ok? ob)
         (define modstr
           (let find-module ([ob ob])
             (if (module? ob)
                 (module->string ob)
                 (ormap find-module (hash-ref graph ob null)))))
         (when modstr
           (hash-update! counts modstr add1 0))))))
  (sort (hash->list counts) > #:key cdr))

(define (get-object-graph proc)
  (define metadata (make-hasheqv))
  (define links (make-hasheqv))
  (define objects
    (call-with-counts
     (lambda (_counts backrefs)
       (define graph (build-graph backrefs))
       (define id-seq 0)
       (define ids (make-hasheq))
       (define (id ob)
         (hash-ref!
          ids ob
          (lambda ()
            (begin0 id-seq
              (set! id-seq (add1 id-seq))))))
       (define (track! ob)
         (define ob-id (id ob))
         (unless (hash-has-key? metadata ob-id)
           (hash-set! metadata ob-id (object-metadata ob ob-id))
           (for ([ref (in-list (hash-ref graph ob null))])
             (define ref-id (id ref))
             (hash-update!
              links ob-id
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

(define (build-graph backrefs)
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
  graph)

(define (get-object-graph/by-type type)
  (get-object-graph (get-predicate type)))

(define (get-predicate type)
  (case type
    [(bignum) bignum?]
    [(box) box?]
    [(bytevector) bytevector?]
    [(continuation) continuation]
    [(ephemeron) ephemeron?]
    [(fxvector) fxvector?]
    [(keyword) keyword?]
    [(pair) pair?]
    [(procedure) procedure?]
    [(stencil-vector) stencil-vector?]
    [(string) string?]
    [(symbol) symbol?]
    [(thread) thread?]
    [(vector) vector?]
    [(weakpair) weak-pair?]
    [(will-executor) will-executor?]
    [else (λ (ob)
            (and (record? ob)
                 (let ([rtd (record-rtd ob)])
                   (eq? (record-type-name rtd) type))))]))

(define (object-metadata ob ob-id)
  (hasheq
   'id ob-id
   'len (object-length ob)
   'str (with-handlers ([exn:fail? (λ (_) "#<unknown>")])
          (if (module? ob)
              (module->string ob)
              (->string ob 255)))
   'hash (equal-hash-code ob)))

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

(define module?
  (get-predicate 'module))

;; UNSAFE: Depends on the structure in "expander/namespace/module.rkt":
;;   * field 0: source-name (#f | string | path)
;;   * field 1: self (module-path-index)
(define (module->string ob)
  (define rtd (record-rtd ob))
  (define source-name ((record-accessor rtd 0) ob))
  (define self ((record-accessor rtd 1) ob))
  (format "#<module at ~.s>" (or source-name (and self (module-path-index-path self)) "<unknown>")))

;; UNSAFE: module-path-index in "common/module-path.rkt":
;;   * field 0: path (#f | string | path)
;;   * field 2: resolved (#f | string | path | symbol)
(define (module-path-index-path ob)
  (define rtd (record-rtd ob))
  (or ((record-accessor rtd 0) ob)
      (format "=~.s" ((record-accessor rtd 2) ob))))

(define (type->string t)
  (symbol->string
   (if (record-type-descriptor? t)
       (record-type-name t)
       t)))

(define (object-length ob)
  (cond
    [(bytevector? ob)
     (bytevector-length ob)]
    [(hash? ob)
     (hash-count ob)]
    [(list? ob)
     (length ob)]
    [(string? ob)
     (string-length ob)]
    [(vector? ob)
     (vector-length ob)]
    [else -1]))
