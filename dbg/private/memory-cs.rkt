#lang racket/base

(require ffi/unsafe/atomic
         ffi/unsafe/vm
         racket/port)

(provide
 get-object-counts)

(define enable-object-counts (vm-primitive 'enable-object-counts))
(define enable-object-backreferences (vm-primitive 'enable-object-backreferences))
(define object-counts (vm-primitive 'object-counts))
(define object-backreferences (vm-primitive 'object-backreferences))

(define (get-counts&backrefs)
  (call-as-atomic
   (lambda ()
     (enable-object-counts #t)
     (enable-object-backreferences #t)
     (collect-garbage)
     (define counts (object-counts))
     (define backrefs (object-backreferences))
     (enable-object-counts #f)
     (enable-object-backreferences #f)
     (values counts backrefs))))

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

(define (object->string o)
  (call-with-output-string
   (lambda (out)
     (write o out))))
