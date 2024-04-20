### Background

This is a (largely abandoned) experiment in building a small "Excel-style" expressions language. The core of the language works, but it only has four keywords implemented (sum, any, all, average).

Example expressions:

```
sum(8,9)
sum(8,99,67,average(sum(87,23),8,67))
any(1,0,0,1,1)
any(0,2)
```

The quickest way to try the language is to use the REPL, the (incomplete) API and app were intended to become a way of using the language from the browser.

The web app is a Elm app, which just displays one table at present. The API is more full-featured. The API allows for tables to be created, overwritten, downloaded, to have rows inserted, and allows for expressions to be evaluated with-or-without the context of a table of data.

### Running the Programs

Before starting, ensure cabal is installed and `cabal update` has been run.

To start the REPL: `cabal run repl`

To start the web server: `cabal run hs`

The Elm webapp builds to a single `index.html`, which was been committed to this repo, and needs no further building. However, if you desire, you can download Elm from https://elm-lang.org/, and build the Elm client by doing `cd elm-client && elm make src/Main.elm`.

The REPL can interpret files containing expressions if they are passed in over stdin: `cabal run repl < file-of-expressions`.

### License

Released into the public domain without any guarantee of any fitness for any purpose whatsoever.
