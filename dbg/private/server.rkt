#lang racket/base

(require racket/match
         racket/tcp
         "common.rkt"
         "gc.rkt")

(provide
 serve)

(define system-info
  (hash-set*
   (for/hasheq ([k (in-list '(os* arch vm))])
     (values k (system-type k)))
   'version (version)))

(define (serve #:host [host "127.0.0.1"]
               #:port [port 9011])
  (define cmd-ch (make-channel))
  (define stop-ch (make-channel))
  (define listener
    (tcp-listen port 128 #t host))
  (define thd
    (thread
     (lambda ()
       (define gc-info-evt
         (make-gc-info-evt))
       (let loop ([subscriptions (hasheq)])
         (sync
          (handle-evt stop-ch void)
          (handle-evt
           listener
           (λ (_)
             (define-values (in out)
               (tcp-accept listener))
             (thread (λ () (handle in out cmd-ch)))
             (loop subscriptions)))
          (handle-evt
           cmd-ch
           (λ (cmd)
             (match cmd
               [`(subscribe ,topic ,ch)
                (loop (hash-update subscriptions topic (λ (chs) (cons ch chs)) null))]

               [`(unsubscribe ,topic ,ch)
                (loop (hash-update subscriptions topic (λ (chs) (remove ch chs)) null))]

               [`(unsubscribe-all ,ch)
                (loop (for/hash ([(topic chs) (in-hash subscriptions)])
                        (values topic (remove ch chs))))]

               [_
                (log-warning "invalid command: ~e" cmd)
                (loop subscriptions)])))
          (handle-evt
           gc-info-evt
           (λ (data)
             (define ts (/ (current-inexact-milliseconds) 1000))
             (for ([ch (in-list (hash-ref subscriptions 'gc null))])
               (sync/timeout 0 (channel-put-evt ch `(gc ,ts ,data))))
             (loop subscriptions))))))))
  (λ ()
    (channel-put stop-ch '(stop))
    (thread-wait thd)))

(define (handle client-in client-out server-ch)
  (define (disconnect)
    (channel-put server-ch `(unsubscribe-all ,async-ch))
    (tcp-abandon-port client-in)
    (tcp-abandon-port client-out))
  (define async-ch
    (make-channel))
  (let loop ()
    (with-handlers ([exn:fail:network? (λ (_e) (disconnect))])
      (sync
       (handle-evt
        async-ch
        (λ (topic&data)
          (write/flush `(async ,@topic&data) client-out)
          (loop)))
       (handle-evt
        client-in
        (λ (_)
          (match (read client-in)
            [(? eof-object?)
             (disconnect)]

            [`(disconnect ,id)
             (write/flush `(bye ,id) client-out)
             (disconnect)]

            [`(subscribe ,id ,topic)
             (channel-put server-ch `(subscribe ,topic ,async-ch))
             (write/flush `(ok ,id) client-out)
             (loop)]

            [`(unsubscribe ,id ,topic)
             (channel-put server-ch `(unsubscribe ,topic ,async-ch))
             (write/flush `(ok ,id) client-out)
             (loop)]

            [`(ping ,id)
             (write/flush `(pong ,id) client-out)
             (loop)]

            [`(info ,id)
             (write/flush `(info ,id ,system-info) client-out)
             (loop)]

            [`(memory-use ,id)
             (write/flush `(memory-use ,id ,(current-memory-use)) client-out)
             (loop)]

            [message
             (write/flush `(error ,(format "invalid message: ~e" message)) client-out)
             (loop)])))))))
