<img src="https://github.com/Bogdanp/racket-dbg/raw/master/media/screenshot.png" alt="screenshot" width="356" height="256" align="right" />

# dbg

A server, client and UI for remotely debugging Racket applications.
Very much a work in progress.

## Usage
### Run a server

```racket
(require (prefix-in dbg: debugging/server))
(define dbg:stop (dbg:serve))
```

### Run the UI

    raco dbg

## License

    dbg is licensed under the 3-Clause BSD license.
