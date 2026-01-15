import glance
import gleam/result
import gloat
import gloat/env
import gloat/type_error
import gloat/types

pub fn simple_exprs_test() {
  assert_infer("True", "Bool")
  assert_infer("False", "Bool")
  assert_infer("1", "Int")
  assert_infer("-2", "Int")
  assert_infer("1.0", "Float")
  assert_infer("-8.0", "Float")
  assert_infer("\"ok\"", "String")
  assert_infer("\"ok\"", "String")
  assert_infer("[]", "List(a)")
  assert_infer("4 % 1", "Int")
  assert_infer("4 > 1", "Bool")
  assert_infer("4 >= 1", "Bool")
  assert_infer("4 <= 1", "Bool")
  assert_infer("4 < 1", "Bool")

  assert_infer("1000_000", "Int")
  assert_infer("1_000", "Int")
  assert_infer("1_000.", "Float")
  assert_infer("10_000.001", "Float")
  assert_infer("100_000.", "Float")

  assert_infer("Nil", "Nil")

  assert_infer("todo", "a")
  assert_infer("1 == todo", "Bool")
  assert_infer("todo != 1", "Bool")
  assert_infer("todo + 1", "Int")
  assert_infer("todo(\"test\") + 1", "Int")

  assert_infer("0xF", "Int")
  assert_infer("0o11", "Int")
  assert_infer("0b1010", "Int")

  assert_infer("6.02e23", "Float")
  assert_infer("6.02e-23", "Float")
}

pub fn assert_patterns_test() {
  assert_infer("let assert [] = [] 1", "Int")
  assert_infer("let assert [a] = [1] a", "Int")
  assert_infer("let assert [a, 2] = [1] a", "Int")
  assert_infer("let assert [a, .._] = [1] a", "Int")
  assert_infer("let assert [a, .._,] = [1] a", "Int")
  assert_infer("fn(x) { let assert [a] = x a }", "fn(List(a)) -> a")
  assert_infer("fn(x) { let assert [a] = x a + 1 }", "fn(List(Int)) -> Int")
  assert_infer("let assert _x = 1 2.0", "Float")
  assert_infer("let assert _ = 1 2.0", "Float")
  assert_infer("let assert #(tag, x) = #(1.0, 1) x", "Int")
  assert_infer("fn(x) { let assert #(a, b) = x a }", "fn(#(a, b)) -> a")
  assert_infer("let assert 5: Int = 5 5", "Int")
}

pub fn lists_test() {
  assert_infer("[]", "List(a)")
  assert_infer("[1]", "List(Int)")
  assert_infer("[1, 2, 3]", "List(Int)")
  assert_infer("[[]]", "List(List(a))")
  assert_infer("[[1.0, 2.0]]", "List(List(Float))")
  assert_infer("[fn(x) { x }]", "List(fn(a) -> a)")
  assert_infer("[fn(x) { x + 1 }]", "List(fn(Int) -> Int)")
  assert_infer("[fn(x) { x }, fn(x) { x + 1 }]", "List(fn(Int) -> Int)")
  assert_infer("[fn(x) { x + 1 }, fn(x) { x }]", "List(fn(Int) -> Int)")
  assert_infer("[[], []]", "List(List(a))")
  assert_infer("[[], [1]]", "List(List(Int))")
  assert_infer("[1, ..[2, ..[]]]", "List(Int)")
  assert_infer("[fn(x) { x }, ..[]]", "List(fn(a) -> a)")
  assert_infer("let x = [1, ..[]] [2, ..x]", "List(Int)")
}

pub fn trailing_comma_lists_test() {
  assert_infer("[1, ..[2, ..[],]]", "List(Int)")
  assert_infer("[fn(x) { x },..[]]", "List(fn(a) -> a)")
  assert_infer("let f = fn(x) { x } [f, f]", "List(fn(a) -> a)")
  assert_infer("[#([], [])]", "List(#(List(a), List(b)))")
}

pub fn tuples_test() {
  assert_infer("#(1)", "#(Int)")
  assert_infer("#(1, 2.0)", "#(Int, Float)")
  assert_infer("#(1, 2.0, 3)", "#(Int, Float, Int)")
  assert_infer("#(1, 2.0, #(1, 1))", "#(Int, Float, #(Int, Int))")
}

pub fn expr_fn_test() {
  assert_infer("fn(x) { x }", "fn(a) -> a")
  assert_infer("fn(x) { x }", "fn(a) -> a")
  assert_infer("fn(x, y) { x }", "fn(a, b) -> a")
  assert_infer("fn(x, y) { [] }", "fn(a, b) -> List(c)")
  assert_infer("let x = 1.0 1", "Int")
  assert_infer("let id = fn(x) { x } id(1)", "Int")
  assert_infer("let x = fn() { 1.0 } x()", "Float")
  assert_infer("fn(x) { x }(1)", "Int")
  assert_infer("fn() { 1 }", "fn() -> Int")
  assert_infer("fn() { 1.1 }", "fn() -> Float")
  assert_infer("fn(x) { 1.1 }", "fn(a) -> Float")
  assert_infer("fn(x) { x }", "fn(a) -> a")
  assert_infer("let x = fn(x) { 1.1 } x", "fn(a) -> Float")
  assert_infer("fn(x, y, z) { 1 }", "fn(a, b, c) -> Int")
  assert_infer("fn(x) { let y = x y }", "fn(a) -> a")
  assert_infer("let id = fn(x) { x } id(1)", "Int")
  assert_infer(
    "let constant = fn(x) { fn(y) { x } } let one = constant(1) one(2.0)",
    "Int",
  )

  assert_infer("fn(f) { f(1) }", "fn(fn(Int) -> a) -> a")
  assert_infer("fn(f, x) { f(x) }", "fn(fn(a) -> b, a) -> b")
  assert_infer("fn(f) { fn(x) { f(x) } }", "fn(fn(a) -> b) -> fn(a) -> b")
  assert_infer(
    "fn(f) { fn(x) { fn(y) { f(x, y) } } }",
    "fn(fn(a, b) -> c) -> fn(a) -> fn(b) -> c",
  )
  assert_infer(
    "fn(f) { fn(x, y) { f(x)(y) } }",
    "fn(fn(a) -> fn(b) -> c) -> fn(a, b) -> c",
  )
  assert_infer(
    "fn(f) { fn(x) { let ff = f ff(x) } }",
    "fn(fn(a) -> b) -> fn(a) -> b",
  )
  assert_infer(
    "fn(f) { fn(x, y) { let ff = f(x) ff(y) } }",
    "fn(fn(a) -> fn(b) -> c) -> fn(a, b) -> c",
  )
  assert_infer("fn(x) { fn(y) { x } }", "fn(a) -> fn(b) -> a")
  assert_infer("fn(f) { f() }", "fn(fn() -> a) -> a")
  assert_infer("fn(f, x) { f(f(x)) }", "fn(fn(a) -> a, a) -> a")
  assert_infer(
    "let id = fn(a) { a } fn(x) { x(id) }",
    "fn(fn(fn(a) -> a) -> b) -> b",
  )

  assert_infer("let add = fn(x, y) { x + y } add(_, 2)", "fn(Int) -> Int")
  assert_infer("fn(x) { #(1, x) }", "fn(a) -> #(Int, a)")
  assert_infer("fn(x, y) { #(x, y) }", "fn(a, b) -> #(a, b)")
  assert_infer("fn(x) { #(x, x) }", "fn(a) -> #(a, a)")
  assert_infer("fn(x) -> Int { x }", "fn(Int) -> Int")
  assert_infer("fn(x) -> a { x }", "fn(a) -> a")
  assert_infer("fn() -> Int { 2 }", "fn() -> Int")
}

pub fn case_test() {
  assert_infer("case 1 { a -> 1 }", "Int")
  assert_infer("case 1 { a -> 1.0 b -> 2.0 c -> 3.0 }", "Float")
  assert_infer("case 1 { a -> a }", "Int")
  assert_infer("case 1 { 1 -> 10 2 -> 20 x -> x * 10 }", "Int")
  assert_infer("case 2.0 { 2.0 -> 1 x -> 0 }", "Int")
  assert_infer("case \"ok\" { \"ko\" -> 1 x -> 0 }", "Int")
}

pub fn multiple_subject_case_test() {
  assert_infer("case 1, 2.0 { a, b -> a }", "Int")
  assert_infer("case 1, 2.0 { a, b -> b }", "Float")
  assert_infer("case 1, 2.0, 3 { a, b, c -> a + c }", "Int")
}

pub fn tuple_index_test() {
  assert_infer("#(1, 2.0).0", "Int")
  assert_infer("#(1, 2.0).1", "Float")
}

pub fn pipe_test() {
  assert_infer("1 |> fn(x) { x }", "Int")
  assert_infer("1.0 |> fn(x) { x }", "Float")
  assert_infer("let id = fn(x) { x } 1 |> id", "Int")
  assert_infer("let id = fn(x) { x } 1.0 |> id", "Float")
  assert_infer("let add = fn(x, y) { x + y } 1 |> add(_, 2)", "Int")
  assert_infer("let add = fn(x, y) { x + y } 1 |> add(2, _)", "Int")
  assert_infer("let add = fn(x, y) { x + y } 1 |> add(2)", "Int")
  assert_infer("let id = fn(x) { x } 1 |> id()", "Int")
  assert_infer("let add = fn(x) { fn(y) { y + x } } 1 |> add(1)", "Int")
  assert_infer(
    "let add = fn(x, _, _) { fn(y) { y + x } } 1 |> add(1, 2, 3)",
    "Int",
  )
}

pub fn bit_array_test() {
  assert_infer("let assert <<x>> = <<1>> x", "Int")
}

pub fn bit_array2_test() {
  assert_infer("let assert <<x>> = <<1>> x", "Int")
}

pub fn bit_array3_test() {
  assert_infer("let assert <<x:float>> = <<1>> x", "Float")
}

pub fn bit_array4_test() {
  assert_infer("let assert <<x:bytes>> = <<1>> x", "BitArray")
}

pub fn bit_array5_test() {
  assert_infer("let assert <<x:bytes>> = <<1>> x", "BitArray")
}

pub fn bit_array6_test() {
  assert_infer("let assert <<x:bits>> = <<1>> x", "BitArray")
}

pub fn bit_array7_test() {
  assert_infer("let assert <<x:bits>> = <<1>> x", "BitArray")
}

pub fn bit_array8_test() {
  assert_infer(
    "let assert <<x:utf8_codepoint>> = <<128013:32>> x",
    "UtfCodepoint",
  )
}

pub fn bit_array9_test() {
  assert_infer(
    "let assert <<x:utf16_codepoint>> = <<128013:32>> x",
    "UtfCodepoint",
  )
}

pub fn bit_array10_test() {
  assert_infer(
    "let assert <<x:utf32_codepoint>> = <<128013:32>> x",
    "UtfCodepoint",
  )
}

pub fn bit_array11_test() {
  assert_infer(
    "let a = <<1>> let assert <<x:bits>> = <<1, a:2-bits>> x",
    "BitArray",
  )
}

pub fn bit_array12_test() {
  assert_infer("let x = <<<<1>>:bits, <<2>>:bits>> x", "BitArray")
}

pub fn let_as_expression_test() {
  assert_infer("let x = 1", "Int")
}

pub fn let_as_expression1_test() {
  assert_infer("let x = { let x = 1 }", "Int")
}

pub fn let_as_expression2_test() {
  assert_infer("let x = { let x = 1. }", "Float")
}

pub fn string_concat_ok_test() {
  assert_infer(" \"1\" <> \"2\" ", "String")
}

fn assert_infer(code: String, expected: String) {
  let module_code = "const top = {\n" <> code <> "\n}"
  assert Ok(expected) == process(module_code, "top")
}

fn process(code, name) {
  result.map(infer_scheme_from_glance(code, name), gloat.scheme_to_string)
}

fn infer_scheme_from_glance(
  code: String,
  name: String,
) -> Result(gloat.Scheme, gloat.TypeError) {
  let assert Ok(parsed) = glance.module(code)
  result.try(gloat.add_module(gloat.builtin_env(), parsed), fn(env_) {
    case env.resolve(env_, name) {
      Ok(scheme) -> Ok(scheme)
      Error(_) ->
        Error(type_error.new("definition not found in env", types.unknown_span))
    }
  })
}
