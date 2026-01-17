import ported_helpers as helpers

pub fn generic_phantom_test() {
  assert Ok(helpers.sort_pairs([#("MakeTest", "fn(Test(Int)) -> Test(a)")]))
    == helpers.assert_module_infer(
      "\npub type Test(a) {\n  MakeTest(field: Test(Int))\n}\n",
    )
}

pub fn generic_record_update1_test() {
  assert Ok(
      helpers.sort_pairs([
        #("Box", "fn(a, Int) -> Box(a)"),
        #("update_box", "fn(Box(Int), String) -> Box(String)"),
      ]),
    )
    == helpers.assert_module_infer(
      "\npub type Box(a) {\n  Box(value: a, i: Int)\n}\n\npub fn update_box(box: Box(Int), value: String) {\n  Box(..box, value: value)\n}",
    )
}

pub fn generic_record_update2_test() {
  assert Ok(
      helpers.sort_pairs([
        #("Box", "fn(a, Int) -> Box(a)"),
        #("update_box", "fn(Box(a), b) -> Box(b)"),
      ]),
    )
    == helpers.assert_module_infer(
      "\npub type Box(a) {\n  Box(value: a, i: Int)\n}\n\npub fn update_box(box: Box(a), value: b) {\n  Box(..box, value: value)\n}",
    )
}

pub fn inferred_variant_record_update_change_type_parameter_test() {
  assert Ok(
      helpers.sort_pairs([
        #("Locked", "fn(String, a) -> Box(a)"),
        #("Unlocked", "fn(String, a) -> Box(a)"),
        #("main", "fn() -> Box(Bool)"),
      ]),
    )
    == helpers.assert_module_infer(
      "\npub type Box(a) {\n  Locked(password: String, value: a)\n  Unlocked(password: String, value: a)\n}\n\npub fn main() {\n  let box = Locked(\"ungu€$$4bLe\", 11)\n  case box {\n    Locked(..) as box -> Locked(..box, value: True)\n    Unlocked(..) as box -> Unlocked(..box, value: False)\n  }\n}\n",
    )
}
