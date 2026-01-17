import ported_helpers as helpers

pub fn echo_has_same_type_as_printed_expression_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> Int")]))
    == helpers.assert_module_infer(
      "\npub fn main() {\n  echo 1\n}\n",
    )
}

pub fn echo_has_same_type_as_printed_expression_2_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> a")]))
    == helpers.assert_module_infer(
      "\npub fn main() {\n  let wibble = todo\n  echo wibble\n}\n",
    )
}

pub fn echo_in_pipeline_acts_as_the_identity_function_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> List(Int)")]))
    == helpers.assert_module_infer(
      "\npub fn main() {\n  [1, 2, 3]\n  |> echo\n}\n",
    )
}

pub fn echo_in_pipeline_acts_as_the_identity_function_2_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> Bool")]))
    == helpers.assert_module_infer(
      "\npub fn main() {\n  1\n  |> echo\n  |> fn(_: Int) { True }\n}\n",
    )
}

pub fn echo_in_pipeline_acts_as_the_identity_function_3_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> List(String)")]))
    == helpers.assert_module_infer(
      "\npub fn main() {\n  [1, 2, 3]\n  |> echo\n  |> echo\n  |> wibble\n}\n\nfn wibble(_: List(Int)) -> List(String) { todo }\n",
    )
}
