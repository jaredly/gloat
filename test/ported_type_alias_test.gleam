import ported_helpers as helpers

pub fn alias_dep_test() {
  assert Ok(helpers.sort_pairs([]))
    == helpers.assert_module_infer(
      "\ntype E = #(F, C)\ntype F = fn(CustomA) -> CustomB(B)\ntype A = Int\ntype B = C\ntype C = CustomA\ntype D = CustomB(C)\n\ntype CustomA {\n  CustomA()\n}\ntype CustomB(a) {\n  CustomB(a)\n}\n",
    )
}

pub fn custom_type_dep_test() {
  assert Ok(helpers.sort_pairs([]))
    == helpers.assert_module_infer(
      "\ntype A {\n    A(Blah)\n}\n\ntype Blah {\n    B(Int)\n}\n",
    )
}

pub fn alias_different_module_test() {
  assert Ok(helpers.sort_pairs([]))
    == helpers.assert_module_infer_with_deps(
      [#("other", "pub type Blah = Bool")],
      "\nimport other\n\ntype Blah = #(other.Blah, other.Blah)\n",
    )
}
