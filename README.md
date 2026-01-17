# gloat

[![Package Version](https://img.shields.io/hexpm/v/gloat)](https://hex.pm/packages/gloat)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gloat/)

A typechecker for gleam code, written in gleam! To be a concrete, practical example of a modern typechecker.

```sh
gleam add gloat@1
```
```gleam
import gloat
import glance
import io

pub fn main() -> Nil {
    # STOPSHIP this is wrong actually
    let assert Ok(expr) = glance.expression("2 + 2")
    let assert Ok(type_) = gloat.infer_expr(gloat.builtin_env(),expr)
    io.println(gloat.type_to_string(type_))
}
```

Further documentation can be found at <https://hexdocs.pm/gloat>.

## CLI

```sh
gleam run -m gloat_cli -- example-files/glexer/src/glexer.gleam example-files/stdlib/src/ example-files/splitter/src/
```


## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```

## Status

- [x] gleam stdlib type checks
- [ ] can type check self and all dependencies
  - [x] splitter
  - [x] glexer
  - [ ] glance
  - [ ] self

## 🤖 Attribution

Jared Forsyth hand-wrote the original version of a hindley-milner+ type checker written in a [custom clojure-like language](https://github.com/jaredly/j3/blob/main/data/tmp/algw-s2.clj).
gtp-5.2-codex proceeded to do almost all of the work thereafter
- porting to gleam
- porting the official gleam compiler's type-checker-tests (written in rust) to gleam
- modifying `gloat`'s type inference implementation such that the tests would pass
- making a cli
