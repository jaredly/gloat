import ported_helpers as helpers

pub fn bool_value_test() {
  assert Ok("Nil") == helpers.assert_infer("\nlet value = True\nassert value\n")
}

pub fn equality_check_test() {
  assert Ok("Nil")
    == helpers.assert_infer("\nlet value = 10\nassert value == 10\n")
}

pub fn comparison_test() {
  assert Ok("Nil")
    == helpers.assert_infer("\nlet value = 4\nassert value < 5\n")
}

pub fn function_call_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> Nil")]))
    == helpers.assert_module_infer(
      "\nfn bool() {\n  True\n}\n\npub fn main() {\n  assert bool()\n}\n",
    )
}

pub fn with_message_test() {
  assert Ok("Nil")
    == helpers.assert_infer("assert True as \"This should never panic\"")
}

pub fn compound_message_test() {
  assert Ok("Nil")
    == helpers.assert_infer(
      "assert 1 == 2 as { \"one\" <> \" is never equal to \" <> \"two\" }",
    )
}
