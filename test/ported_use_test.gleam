import ported_helpers as helpers

pub fn arity_1_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> #(Int, Int)")]))
    == helpers.assert_module_infer(
      "\npub fn main() {\n  use <- pair()\n  123\n}\n\nfn pair(f) {\n  let x = f()\n  #(x, x)\n}\n",
    )
}

pub fn arity_2_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> #(Float, Int)")]))
    == helpers.assert_module_infer(
      "\npub fn main() {\n  use <- pair(1.0)\n  123\n}\n\nfn pair(x, f) {\n  let y = f()\n  #(x, y)\n}\n",
    )
}

pub fn arity_3_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> #(Float, String, Int)")]))
    == helpers.assert_module_infer(
      "\npub fn main() {\n  use <- trip(1.0, \"\")\n  123\n}\n\nfn trip(x, y, f) {\n  let z = f()\n  #(x, y, z)\n}\n",
    )
}

pub fn call_is_variable_test() {
  assert Ok("Int")
    == helpers.assert_infer("\nlet call = fn(f) { f() }\nuse <- call\n123\n")
}

pub fn call_is_literal_test() {
  assert Ok("Float") == helpers.assert_infer("\nuse <- fn(f) { f() }\n123.0\n")
}

pub fn call_is_capture_test() {
  assert Ok("Int")
    == helpers.assert_infer(
      "\nlet f = fn(a, b) { a() + b }\nuse <- f(_, 123)\n123\n",
    )
}

pub fn discard_test() {
  assert Ok("Nil")
    == helpers.assert_infer("\nlet x = fn(f) { f(123) }\nuse _ <- x()\nNil\n")
}

pub fn discard_named_test() {
  assert Ok("Nil")
    == helpers.assert_infer(
      "\nlet x = fn(f) { f(123) }\nuse _wibble <- x()\nNil\n",
    )
}

pub fn labels_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> Int")]))
    == helpers.assert_module_infer(
      "\npub fn main() {\n  use x <- apply(arg: 1)\n  x\n}\n\nfn apply(fun fun, arg arg) {\n  fun(arg)\n}\n",
    )
}

pub fn patterns_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> Int")]))
    == helpers.assert_module_infer(
      "\npub fn main() {\n  use Box(x) <- apply(Box(1))\n  x\n}\n\ntype Box(a) {\n  Box(a)\n}\n\nfn apply(arg, fun) {\n  fun(arg)\n}\n",
    )
}

pub fn multiple_patterns_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> Int")]))
    == helpers.assert_module_infer(
      "\npub fn main() {\n  use Box(x), Box(y), Box(z) <- apply(Box(1))\n  x + y + z\n}\n\ntype Box(a) {\n  Box(a)\n}\n\nfn apply(arg, fun) {\n  fun(arg, arg, arg)\n}\n",
    )
}

pub fn typed_pattern_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> Int")]))
    == helpers.assert_module_infer(
      "\npub fn main() {\n  use Box(x): Box(Int), Box(y), Box(z) <- apply(Box(1))\n  x + y + z\n}\n\ntype Box(a) {\n  Box(a)\n}\n\nfn apply(arg, fun) {\n  fun(arg, arg, arg)\n}\n",
    )
}
