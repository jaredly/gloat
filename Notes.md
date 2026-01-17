
- [x] we've now got all of gleam stdlib typing!
- [ ] I probably want some more

can you run `gleam test` and summarize the errors you see? we've ported over a bunch of tests from the rust implementation of the gleam type checker, and we're developing `gloat`, a gleam implementation of the same type checker, trying to get it to pass all of the tests.

#

What does the API look like?

-> obviously, a "given this gleam source snippet, do a type check" is a nice basic thing
-> but also, something that works with modules as you can imagine
-> and a cli that does the modules thing
-> anything else? Seems like exposing a way to inspect the type of any given expression would be good too. by span, I expect.
  -> so we could have a "type check gleam in the browser" as a laugh
  -> looks like the wasm version of the gleam compiler is 4mb? while the native version is 19mb. pretty impressive.
