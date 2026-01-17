import ported_helpers as helpers

pub fn empty_list_test() {
  assert Ok(
      helpers.sort_pairs([
        #("a", "fn() -> fn(a) -> Nil"),
        #("b", "fn(a) -> fn(b) -> Nil"),
        #("c", "fn() -> Nil"),
      ]),
    )
    == helpers.assert_module_infer(
      "\npub fn a() {\n  fn(_) { Nil }\n}\n\npub fn b(_) {\n  fn(_) { Nil }\n}\n\npub fn c() {\n  Nil\n  |> b(\n    Nil\n    |> a(),\n  )\n}\n",
    )
}

pub fn pipe_rewrite_with_missing_argument_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> fn(Int) -> Int")]))
    == helpers.assert_module_infer(
      "\npub fn main() {\n  let f = fn(a, b) { fn(c) { a + b + c } }\n  1 |> f(2)\n}\n",
    )
}

pub fn pipe_regression_gh3515_test() {
  assert Ok(
      helpers.sort_pairs([
        #("k_relu", "fn(Int) -> fn(Float) -> fn(String) -> Float"),
      ]),
    )
    == helpers.assert_module_infer(
      "\nfn relu(t) {\n  fn(theta: String) {\n    0.0\n  }\n}\n\npub fn k_relu(k: Int) {\n  fn(t: Float) {\n    fn(theta: String) {\n      case k {\n        0 -> t\n        _ -> {\n          let next_layer = theta |> relu(t) |> k_relu(k - 1)\n          theta |> next_layer\n        }\n      }\n    }\n  }\n}\n",
    )
}

pub fn pipe_callback_var_function1_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> fn(a) -> #(Int, a)")]))
    == helpers.assert_module_infer(
      "\npub fn main() {\n  let f = fn(a) { fn(b) { #(a, b) } }\n  let x = 1 |> f()\n}\n",
    )
}

pub fn pipe_callback_var_function2_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> #(Int, Int)")]))
    == helpers.assert_module_infer(
      "\npub fn main() {\n  let f = fn(a) { fn(b) { #(a, b) } }\n  let x = 1 |> f(1)\n}\n",
    )
}

pub fn pipe_callback_correct_arity1_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> fn() -> String")]))
    == helpers.assert_module_infer(
      "\nfn callback(a: Int) {\n  fn() -> String {\n    \"Called\"\n  }\n}\n\npub fn main() {\n  let x = 1 |> callback()\n}\n",
    )
}

pub fn pipe_callback_correct_arity2_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> String")]))
    == helpers.assert_module_infer(
      "\nfn callback(a: Float) {\n  fn(b: Int) -> String {\n    \"Called\"\n  }\n}\n\npub fn main() {\n  let x = 1 |> callback(2.5)\n}\n",
    )
}
