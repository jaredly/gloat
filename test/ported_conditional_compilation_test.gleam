import ported_helpers as helpers

pub fn excluded_error_test() {
  assert Ok(helpers.sort_pairs([#("x", "Int")]))
    == helpers.assert_module_infer(
      "@target(javascript)\npub type X = Y\n\npub const x = 1\n",
    )
}

pub fn alias_test() {
  assert Ok(helpers.sort_pairs([#("x", "Int")]))
    == helpers.assert_module_infer(
      "@target(erlang)\npub type X = Int\n\npub const x: X = 1\n",
    )
}

pub fn alias_in_block_test() {
  assert Ok(helpers.sort_pairs([#("x", "Int")]))
    == helpers.assert_module_infer(
      "@target(erlang)\npub type X = Int \n\n@target(erlang)\npub const x: X = 1\n",
    )
}

pub fn generalising_test() {
  assert Ok(helpers.sort_pairs([#("id", "fn(a) -> a"), #("x", "fn() -> Int")]))
    == helpers.assert_module_infer(
      "\n@target(erlang)\npub fn id(x) { x }\n\n@target(erlang)\npub fn x() { id(1) }\n",
    )
}

pub fn excluded_generalising_test() {
  assert Ok(
      helpers.sort_pairs([
        #("id", "fn(a) -> a"),
        #("x", "fn() -> Int"),
        #("y", "Int"),
      ]),
    )
    == helpers.assert_module_infer(
      "\n@target(javascript)\npub fn id(x) { x }\n\n@target(javascript)\npub fn x() { id(1) }\n\npub const y = 1\n",
    )
}

pub fn included_const_ref_earlier_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> Int")]))
    == helpers.assert_module_infer(
      "\n@target(erlang)\nconst x = 1\n\npub fn main() { x }\n",
    )
}

pub fn included_const_ref_later_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> Int")]))
    == helpers.assert_module_infer(
      "pub fn main() { x }\n\n@target(erlang)\nconst x = 1\n",
    )
}

pub fn target_does_not_need_to_be_the_first_attribute_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> Int")]))
    == helpers.assert_module_infer(
      "\n@external(erlang, \"blah\", \"wub\")\n@target(erlang)\npub fn main() -> Int\n",
    )
}
