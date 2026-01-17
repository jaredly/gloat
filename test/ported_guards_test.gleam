import ported_helpers as helpers

pub fn nested_record_access_test() {
  assert Ok(
      helpers.sort_pairs([
        #("A", "fn(B) -> A"),
        #("B", "fn(C) -> B"),
        #("C", "fn(Bool) -> C"),
        #("a", "fn(A) -> Int"),
      ]),
    )
    == helpers.assert_module_infer(
      "\npub type A {\n  A(b: B)\n}\n\npub type B {\n  B(c: C)\n}\n\npub type C {\n  C(d: Bool)\n}\n\npub fn a(a: A) {\n  case a {\n    _ if a.b.c.d -> 1\n    _ -> 0\n  }\n}\n",
    )
}

pub fn qualified_record_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn(Wibble) -> Bool")]))
    == helpers.assert_module_infer_with_deps(
      [#("wibble", "pub type Wibble { Wibble Wobble }")],
      "\nimport wibble\n\npub fn main(wibble: wibble.Wibble) {\n  case wibble {\n    w if w == wibble.Wobble -> True\n    _ -> False\n  }\n}\n",
    )
}

pub fn qualified_record_with_arguments_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn(Wibble) -> Bool")]))
    == helpers.assert_module_infer_with_deps(
      [#("wibble", "pub type Wibble { Wibble(Int) Wobble(Int, Float) }")],
      "\nimport wibble\n\npub fn main(wibble: wibble.Wibble) {\n  case wibble {\n    w if w == wibble.Wobble(1, 3.8) -> True\n    _ -> False\n  }\n}\n",
    )
}
