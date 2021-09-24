# dbg

A server, client and UI for remotely debugging Racket applications.
Very much a work in progress.

<center>
  <img src="https://github.com/Bogdanp/racket-dbg/raw/master/media/screenshot.png" alt="screenshot" width="600" height="400" />
</center>

## Usage
### Run a server

```racket
(require debugging/server)
(serve)
```

### Run the UI

    racket -l debugging/ui

## License

    dbg is licensed under the 3-Clause BSD license.
