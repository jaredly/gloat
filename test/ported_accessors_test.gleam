import ported_helpers as helpers

pub fn bug_3629_test() {
  assert Ok(
      helpers.sort_pairs([
        #("One", "fn(Wibble) -> Exp"),
        #("Two", "fn(Wibble) -> Exp"),
        #("main", "fn() -> Wibble"),
      ]),
    )
    == helpers.assert_module_infer_with_deps(
      [#("imported", "pub type Wibble")],
      "\nimport imported\n\npub type Exp {\n  One(field: imported.Wibble)\n  Two(field: imported.Wibble)\n}\n\npub fn main() {\n  let exp = One(todo)\n  exp.field\n}\n",
    )
}
