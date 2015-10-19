the_iron_searcher
=================

Code search tool similar to [ag](http://geoff.greer.fm/ag/) or [ack](http://beyondgrep.com) written in Haskell.

It's a *WORK IN PROGRESS* and now it's only able to search through all files in the specified directory recursively, so:

- Filtering by extensions (`*.hs`, `*.rb`, `*.c`, etc) is not supported yet.
- Ignoring patterns from `.gitignore` and `.hgignore` is not supported yet.
- Passing multiple paths in command line arguments is not supported yet.

# Development

For developing you need cabal and ghc [installed](https://github.com/bitemyapp/learnhaskell/blob/master/install.md).

Initialize sandbox:
```
$ cd the_iron_searcher
$ cabal sandbox init
```
Build:
```
$ cabal install -j
```
REPL:
```
$ cabal repl
```
Run:
```
$ .cabal-sandbox/bin/the-iron-searcher rb_thread_call_without_gvl ~/code/ruby
```
Remind that it's convenient to add `.cabal-sandbox/bin` to `$PATH` in your `.bashrc` / `.zshrc`:
```
PATH=$PATH:.cabal-sandbox/bin
```
