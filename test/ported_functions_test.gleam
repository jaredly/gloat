import ported_helpers as helpers

pub fn all_labelled_test() {
  assert Ok(helpers.sort_pairs([#("prepend", "fn(List(a), a) -> List(a)")]))
    == helpers.assert_module_infer(
      "\npub fn prepend(to list: List(a), this item: a) -> List(a) {\n  [item, ..list]\n}\n",
    )
}

pub fn out_of_order_generalisation_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> String")]))
    == helpers.assert_module_infer(
      "\npub fn main() {\n  call(fn() {\n    \"Hello\"\n  })\n}\n\nfn call(f: fn() -> a) {\n  f()\n}\n",
    )
}

pub fn bug_2275_test() {
  assert Ok(helpers.sort_pairs([#("zero", "fn() -> Nil")]))
    == helpers.assert_module_infer(
      "\npub fn zero() {\n  one()\n}\n\nfn one() {\n  one()\n  two()\n}\n\nfn two() {\n  two\n  Nil\n}\n",
    )
}

pub fn bug_2275_2_self_references_test() {
  assert Ok(helpers.sort_pairs([#("zero", "fn() -> Nil")]))
    == helpers.assert_module_infer(
      "\npub fn zero() {\n  one()\n}\n\nfn one() {\n  one()\n  two()\n}\n\nfn two() {\n  two\n  two\n  Nil\n}\n",
    )
}

pub fn bug_2275_again_test() {
  assert Ok(
      helpers.sort_pairs([
        #("aaa", "fn(Int) -> Int"),
        #("bbb", "fn() -> Int"),
        #("ccc", "fn() -> Int"),
      ]),
    )
    == helpers.assert_module_infer(
      "\npub fn aaa(input) {\n  case [] {\n    [] -> input\n\n    _ -> {\n      let input2 = bbb()\n      aaa(input2)\n    }\n  }\n}\n\npub fn bbb() {\n  ccc() + bbb()\n}\n\npub fn ccc() {\n  ccc() + bbb()\n}\n",
    )
}

pub fn deprecated_function_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> Nil")]))
    == helpers.assert_module_infer(
      "\n@deprecated(\"use wibble instead\")\npub fn main() {\n  Nil\n}\n",
    )
}

pub fn provide_arg_type_to_fn_implicit_ok_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> Int")]))
    == helpers.assert_module_infer(
      "\npub fn main() {\n   let z = #(1,2)\n   fn(x) { x.0 }(z)\n}\n",
    )
}

pub fn provide_arg_type_to_fn_explicit_ok_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> Int")]))
    == helpers.assert_module_infer(
      "\npub fn main() {\n   let z = #(1,2)\n   fn(x: #(Int, Int)) { x.0 }(z)\n}\n",
    )
}

pub fn provide_two_args_type_to_fn_test() {
  assert Ok(helpers.sort_pairs([#("main", "fn() -> Int")]))
    == helpers.assert_module_infer(
      "\npub fn main() {\n   let a = #(1,2)\n   let b = #(1,2)\n   fn(x, y) { x.0 + y.1 }(a, b)\n}\n",
    )
}
