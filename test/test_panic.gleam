import gleam/dynamic.{type Dynamic}

@external(erlang, "typechecker_test_panic_ffi", "catch_panic")
@external(javascript, "./gloat_cli.mjs", "catch_panic")
fn catch_ffi(f: fn() -> a) -> Result(a, Dynamic)

pub fn catches_panic(f: fn() -> a) -> Bool {
  case catch_ffi(f) {
    Ok(_) -> False
    Error(_) -> True
  }
}
