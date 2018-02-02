# crossword

A [re-frame](https://github.com/Day8/re-frame) application to play crosswords (from the NYT).

See https://s3.amazonaws.com/nikolaperic-public/crossword/index.html to play demo or clone and make your own tweaks.

## Development Mode

### Run application:

```
lein clean
lein figwheel dev
```

Figwheel will automatically push cljs changes to the browser.

Wait a bit, then browse to [http://localhost:3449](http://localhost:3449).

## Production Build


To compile clojurescript to javascript:

```
lein clean
lein cljsbuild once min
```
