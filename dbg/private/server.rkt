#lang racket/base

(require profile/analyzer
         profile/render-json
         profile/sampler
         racket/match
         racket/os
         racket/tcp
         "common.rkt"
         "memory.rkt")

(provide
 serve)

(define current-root-custodian
  (make-parameter #f))

(define current-prof-custodian
  (make-parameter #f))

(define system-info
  (hash-set*
   (for/hasheq ([k (in-list '(os* arch vm))])
     (values k (system-type k)))
   'version (version)
   'pid (getpid)
   'hostname (gethostname)))

(define (serve #:host [host "127.0.0.1"]
               #:port [port 9011])
  (define cust (make-custodian))
  (current-root-custodian (current-custodian))
  (current-prof-custodian cust)
  (current-custodian cust)
  (define cmd-ch (make-channel))
  (define stop-ch (make-channel))
  (define server-cust (make-custodian))
  (parameterize ([current-custodian server-cust])
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
      (thread-wait thd)
      (custodian-shutdown-all server-cust))))

(define (handle client-in client-out server-ch)
  (define (disconnect)
    (channel-put server-ch `(unsubscribe-all ,async-ch))
    (tcp-abandon-port client-in)
    (tcp-abandon-port client-out))
  (define async-ch
    (make-channel))
  (define message-evt
    (handle-evt client-in read))
  (parameterize ([current-output-port client-out])
    (let loop ([s (make-state)])
      (with-handlers ([exn:fail:network? (λ (_e) (disconnect))])
        (sync
         (handle-evt
          async-ch
          (λ (topic&data)
            (write/flush `(async ,@topic&data))
            (loop s)))
         (handle-evt
          message-evt
          (match-lambda
            [(? eof-object?)
             (disconnect)]

            [`(disconnect ,id)
             (write/flush `(bye ,id))
             (disconnect)]

            [`(subscribe ,id ,topic)
             (channel-put server-ch `(subscribe ,topic ,async-ch))
             (write/flush `(ok ,id))
             (loop s)]

            [`(unsubscribe ,id ,topic)
             (channel-put server-ch `(unsubscribe ,topic ,async-ch))
             (write/flush `(ok ,id))
             (loop s)]

            [`(ping ,id)
             (write/flush `(pong ,id))
             (loop s)]

            [`(get-info ,id)
             (write/flush `(info ,id ,system-info))
             (loop s)]

            [`(get-memory-use ,id)
             (write/flush `(memory-use ,id ,(current-memory-use)))
             (loop s)]

            [`(get-object-counts ,id)
             (write/flush `(object-counts ,id ,(get-object-counts)))
             (loop s)]

            [`(get-reference-graph ,id ,type)
             (with-handlers ([exn:fail? (λ (e) (write/flush `(error ,id ,(exn-message e))))])
               (write/flush `(object-graph ,id ,(get-object-graph/by-type type))))
             (loop s)]

            [`(start-profile ,id ,delay-ms ,errortrace?)
             (cond
               [(state-sampler s)
                (write/flush `(error ,id "a profile is already running"))
                (loop s)]

               [else
                (define sampler
                  (create-sampler
                   (current-prof-custodian)
                   (/ delay-ms 1000.0)
                   (current-root-custodian)
                   #:use-errortrace? errortrace?))
                (write/flush `(ok ,id))
                (loop (set-sampler s sampler))])]

            [`(get-profile ,id)
             (cond
               [(state-sampler s)
                (write/flush `(ok ,id ,(get-profile s)))
                (loop s)]

               [else
                (write/flush `(error ,id "a profile is not currently running"))
                (loop s)])]

            [`(stop-profile ,id)
             (cond
               [(state-sampler s)
                => (λ (sampler)
                     (sampler 'stop)
                     (write/flush `(ok ,id ,(get-profile s)))
                     (loop (clear-sampler s)))]

               [else
                (write/flush `(error ,id "a profile is not currently running"))
                (loop s)])]

            [`(,cmd ,id ,args ...)
             (write/flush `(error ,id ,(format "invalid message: ~e" `(,cmd ,@args))))
             (loop s)]

            [message
             (write/flush `(error ,(format "invalid message: ~e" message)))
             (loop s)])))))))


;; state ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct state (sampler)
  #:transparent)

(define (make-state)
  (state #f))

(define (set-sampler s sampler)
  (struct-copy state s [sampler sampler]))

(define (clear-sampler s)
  (struct-copy state s [sampler #f]))

(define (get-profile s)
  (define snapshots ((state-sampler s) 'get-snapshots))
  (profile->json (analyze-samples snapshots)))
