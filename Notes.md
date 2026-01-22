


#

What does the API look like?

-> obviously, a "given this gleam source snippet, do a type check" is a nice basic thing
-> but also, something that works with modules as you can imagine
-> and a cli that does the modules thing
-> anything else? Seems like exposing a way to inspect the type of any given expression would be good too. by span, I expect.
  -> so we could have a "type check gleam in the browser" as a laugh
  -> looks like the wasm version of the gleam compiler is 4mb? while the native version is 19mb. pretty impressive.

# Let's self-host!

- glance has some errors
  - only specifying a single label for a 3-arg record
  - my order of operations for type inference isn't working
    with `use` being treated as just a function.

#

OH the cli needs to just use `build/packages/*` right?
