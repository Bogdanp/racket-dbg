#lang scribble/manual

@(require (for-label debugging/client
                     debugging/server
                     racket/base
                     racket/contract))

@title{@tt{dbg}: debug applications remotely}
@author[@(author+email "Bogdan Popa" "bogdan@defn.io")]

This package provides a server, client and UI for remotely debugging
Racket applications.

@section{Usage}

Call @racket[serve] before starting your app and use the returned
function to stop the server when your app shuts down.

@racketblock[
(require (prefix-in dbg: debugging/server))
(define dbg:stop (dbg:serve))
]

Then use the UI to interact with the server (@tt{raco dbg} from the
@tt{dbg-ui} package).  If you're feeling particularly adventurous, you
can use the programmatic API (@secref{Client}) instead.

@subsection{Port Forwarding}

The server listens on the loopback interface by default, so it's not
exposed to the internet.  While you can tell it to listen on a
different interface, that would likely be insecure.  Instead, when you
need to debug a remote server, you should use port forwarding.  Here's
how you would forward port @tt{9011} using the @tt{ssh} command:

@verbatim{ssh -L 9011:127.0.0.1:9011 example.com}


@section{Reference}

The @tt{dbg} package provides two top-level modules, representing the
@secref{Server} and @secref{Client} APIs.

@subsection{Server}
@defmodule[debugging/server]

@defproc[(serve [#:host host string? "127.0.0.1"]
                [#:port port (integer-in 0 65535) 9011]
                [#:custodian custodian custodian? (current-custodian)]) (-> void?)]{

  Runs a background debugging server bound to @racket[port] on
  @racket[host] and returns a function that stops the server when
  called.

  The server replaces the @racket[current-custodian] in order to
  aid with profiling, so the earlier you start the server during
  your app's boot process, the better. Alternatively, you may pass a
  @racket[#:custodian] to ensure that profiling and thread dumping
  can access threads and other custodians belonging to it and its
  descendants.

  @history[#:changed "0.3" @elem{Added the @racket[#:custodian] argument.}]
}

@subsection{Client}
@defmodule[debugging/client]

The client API may change between versions without warning.

@defparam[current-client client client?]{
  A parameter that holds the current client.
}

@defproc[(client? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is a client.
}

@defproc[(connect [#:host host string? "127.0.0.1"]
                  [#:port port (integer-in 0 65535) 9011]) client?]{

  Connects to the debugging server at the given @racket[host] and
  @racket[port].
}

@defproc[(connected? [c client? (current-client)]) boolean?]{
  Returns @racket[#t] when @racket[c] is connected.
}

@defproc[(disconnect! [c client? (current-client)]) void?]{
  Disconnects @racket[c] from the server.
}

@defproc[(reconnect! [c client? (current-client)]) void?]{
  Reconnects @racket[c] to the server.
}

@defproc[(get-info [c client? (current-client)]) hash?]{
  Gets metadata about the process being debugged.
}

@defproc[(get-memory-use [c client? (current-client)]) exact-positive-integer?]{
  Gets the number of allocated bytes in use by the process being
  debugged.
}

@defproc[(get-object-counts [c client? (current-client)]) (listof (cons/c string? (cons/c integer? integer?)))]{
  Gets the current number and size (in bytes) of objects alloced by
  the process being debugged, grouped by type.
}

@defproc[(start-profile [c client? (current-client)]
                        [delay-ms exact-nonnegative-integer? 1]
                        [errortrace? boolean? #f]) void?]{

  Starts a profiling the process being debugged, sampling every
  @racket[delay-ms] milliseconds.

  The @racket[errortrace?] argument controls whether or not
  errortrace is used to construct call stacks.  If errortrace was not
  enabled before starting the process, setting this value to
  @racket[#t] will produce empty profiles.
}

@defproc[(stop-profile [c client? (current-client)]) any/c]{
  Stops the current in-progress profile and returns the profile.
}

@defproc[(get-profile [c client? (current-client)]) any/c]{
  Gets the last profile from the process being debugged.
}

@defproc[(dump-threads [c client? (current-client)]) string?]{
  Returns a string representing all the threads accessible via the
  server's custodian and their stack frames.

  @history[#:added "0.3"]
}
