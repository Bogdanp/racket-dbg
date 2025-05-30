#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         profile/render-json
         racket/match
         racket/tcp
         "common.rkt"
         "error.rkt")

(provide
 current-client
 client?
 connect
 connected?
 reconnect!
 disconnect!
 subscribe
 unsubscribe
 async-evt
 get-info
 get-memory-use
 get-object-counts
 get-reference-graph
 start-profile
 stop-profile
 get-profile
 dump-threads)

(define current-client
  (make-parameter #f))

(struct client
  (host
   port
   async-ch
   [io #:mutable]
   [manager-thd #:mutable]))

(define (oops fmt . args)
  (exn:fail:dbg:client (apply format fmt args) (current-continuation-marks)))

(struct Cmd (id res-ch nack-evt))
(struct Req Cmd ())
(struct Rep Cmd (data))

(define (Cmd->Rep cmd data)
  (Rep (Cmd-id cmd)
       (Cmd-res-ch cmd)
       (Cmd-nack-evt cmd)
       data))

(define-match-expander cmd
  (λ (stx)
    (syntax-parse stx
      [(_ cmds:expr id:expr)
       #'(app (λ (the-id)
                (findf
                 (λ (c)
                   (equal? (Cmd-id c) the-id))
                 cmds))
              (? values id))])))

(define (connect #:host [host "127.0.0.1"]
                 #:port [port 9011])
  (define-values (in out)
    (tcp-connect host port))
  (define async-ch (make-channel))
  (define manager-thd
    (start-manager-thread async-ch in out))
  (client host port async-ch (list in out) manager-thd))

(define (start-manager-thread async-ch in out)
  (thread/suspend-to-kill
   (lambda ()
     (define data-evt (handle-evt in read))
     (let loop ([connected? #t] [seq 0] [cmds null])
       (with-handlers ([exn:break:terminate? void]
                       [exn:fail:network?
                        (λ (e)
                          (log-error "connection error: ~a" (exn-message e))
                          (tcp-abandon-port in)
                          (tcp-abandon-port out)
                          (loop #f seq cmds))])
         (apply
          sync
          (handle-evt
           (thread-receive-evt)
           (λ (_)
             (match (thread-receive)
               [`(,name ,args ... ,res-ch ,nack-evt)
                #:when connected?
                (define req (Req seq res-ch nack-evt))
                (write/flush `(,name ,seq ,@args) out)
                (loop connected? (add1 seq) (cons req cmds))]

               [`(,_ ,_ ... ,res-ch ,nack-evt)
                (define err (oops "client: not connected"))
                (define rep (Rep seq res-ch nack-evt err))
                (loop connected? (add1 seq) (cons rep cmds))])))

          (handle-evt
           (if connected? data-evt never-evt)
           (match-lambda
             [(? eof-object?)
              (tcp-abandon-port in)
              (tcp-abandon-port out)
              (loop #f seq cmds)]

             [`(async ,topic ,ts ,data)
              (sync/timeout 0 (channel-put-evt async-ch `(,topic ,ts ,data)))
              (loop connected? seq cmds)]

             [`(error ,(cmd cmds req) ,message)
              (define err (oops "~a" message))
              (define rep (Cmd->Rep req err))
              (loop connected? seq (cons rep (remq req cmds)))]

             [`(,message ,(cmd cmds req) ,args ...)
              (define rep (Cmd->Rep req `(,message ,@args)))
              (loop connected? seq (cons rep (remq req cmds)))]

             [`(error ,message)
              (log-error "orphan error: ~e" message)
              (loop connected? seq cmds)]))

          (append
           (for/list ([cmd (in-list cmds)])
             (if (Rep? cmd)
                 (handle-evt
                  (channel-put-evt
                   (Cmd-res-ch cmd)
                   (Rep-data cmd))
                  (λ (_)
                    (loop connected? seq (remq cmd cmds))))
                 never-evt))

           (for/list ([cmd (in-list cmds)])
             (handle-evt
              (Cmd-nack-evt cmd)
              (λ (_)
                (loop connected? seq (remq cmd cmds))))))))))))

(define (connected? [c (current-client)])
  (andmap (compose1 not port-closed?) (client-io c)))

(define (reconnect! [c (current-client)])
  (when (connected? c)
    (disconnect! c))
  (match-define (client host port async-ch _io _manager-thd) c)
  (define-values (in out)
    (tcp-connect host port))
  (define manager-thd
    (start-manager-thread async-ch in out))
  (set-client-io! c (list in out))
  (set-client-manager-thd! c manager-thd))

(define (do-send c cmd)
  (define thd (client-manager-thd c))
  (define res-ch (make-channel))
  (handle-evt
   (nack-guard-evt
    (λ (nack-evt)
      (begin0 res-ch
        (thread-resume thd)
        (thread-send thd `(,@cmd ,res-ch ,nack-evt)))))
   (λ (v-or-exn)
     (if (exn:fail:dbg:client? v-or-exn)
         (raise v-or-exn)
         v-or-exn))))

(define-syntax-rule (send c cmd arg ...)
  (do-send c (list 'cmd arg ...)))

(define-syntax-rule (send/sync c cmd arg ...)
  (sync (send c cmd arg ...)))

(define (async-evt [c (current-client)])
  (client-async-ch c))

(define (disconnect! [c (current-client)])
  (void (send/sync c disconnect))
  (break-thread (client-manager-thd c) 'terminate))

(define subscribe
  (case-lambda
    [(topic) (subscribe (current-client) topic)]
    [(c topic) (void (send/sync c subscribe topic))]))

(define unsubscribe
  (case-lambda
    [(topic) (unsubscribe (current-client) topic)]
    [(c topic) (void (send/sync c unsubscribe topic))]))

(define (get-info [c (current-client)])
  (cadr (send/sync c get-info)))

(define (get-memory-use [c (current-client)])
  (cadr (send/sync c get-memory-use)))

(define (get-object-counts [c (current-client)])
  (cadr (send/sync c get-object-counts)))

(define (get-reference-graph type [c (current-client)])
  (cadr (send/sync c get-reference-graph type)))

(define (start-profile [c (current-client)] [delay-ms 1] [errortrace? #f])
  (void (send/sync c start-profile delay-ms errortrace?)))

(define (stop-profile [c (current-client)])
  (json->profile (cadr (send/sync c stop-profile))))

(define (get-profile [c (current-client)])
  (json->profile (cadr (send/sync c get-profile))))

(define (dump-threads [c (current-client)])
  (cadr (send/sync c dump-threads)))
