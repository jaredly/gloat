# gloat

[![Package Version](https://img.shields.io/hexpm/v/gloat)](https://hex.pm/packages/gloat)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gloat/)

A typechecker for gleam code, written in gleam! To be a concrete, practical example of a modern typechecker.

```sh
gleam add gloat@1
```
```gleam
import gloat
import io

pub fn main() -> Nil {
  let assert Ok(type_) = gloat.infer_expr(gloat.builtin_env(), "2 + 2")
  io.println(gloat.type_to_string(type_))
}
```

Further documentation can be found at <https://hexdocs.pm/gloat>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```

## Status

- [x] gleam stdlib type checks
