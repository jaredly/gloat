import ported_helpers as helpers

pub fn import_value_with_same_name_as_imported_module_test() {
  assert Ok(helpers.sort_pairs([#("a", "Int")]))
    == helpers.assert_module_infer_with_deps(
      [#("other", "pub const other = 1")],
      "\nimport other.{other}\npub const a = other\n",
    )
}

pub fn imported_constant_record_test() {
  assert Ok(helpers.sort_pairs([#("a", "Thing")]))
    == helpers.assert_module_infer_with_deps(
      [#("one/two", "pub type Thing { Thing(Int) }")],
      "\nimport one/two\n\npub const a = two.Thing(1)\n",
    )
}

pub fn import_type_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> Int")]))
    == helpers.assert_module_infer_with_deps(
      [#("one", "pub type One = Int")],
      "import one.{type One}\n\npub fn main() -> One {\n  todo\n}\n",
    )
}

pub fn deprecated_type_import_conflict_test() {
  assert Ok(helpers.sort_pairs([]))
    == helpers.assert_module_infer_with_deps(
      [#("one", "pub type X { X }")],
      "import one.{X, type X}",
    )
}

pub fn aliased_unqualified_type_and_value_test() {
  assert Ok(helpers.sort_pairs([]))
    == helpers.assert_module_infer_with_deps(
      [#("one", "pub type X { X }")],
      "import one.{X as XX, type X as XX}",
    )
}

pub fn deprecated_type_import_conflict_two_modules_test() {
  assert Ok(helpers.sort_pairs([]))
    == helpers.assert_module_infer_with_deps(
      [
        #("one", "pub type X { X }"),
        #("two", "pub type X { X }"),
      ],
      "\nimport one.{type X as Y}\nimport two.{X}\n",
    )
}
