import glance
import gleam/result
import gloat
import gloat/env
import gloat/type_error
import gloat/types

pub fn simple_exprs_test() {
  assert Ok("Bool") == assert_infer("True")
  assert Ok("Bool") == assert_infer("False")
  assert Ok("Int") == assert_infer("1")
  assert Ok("Int") == assert_infer("-2")
  assert Ok("Float") == assert_infer("1.0")
  assert Ok("Float") == assert_infer("-8.0")
  assert Ok("String") == assert_infer("\"ok\"")
  assert Ok("String") == assert_infer("\"ok\"")
  assert Ok("List(a)") == assert_infer("[]")
  assert Ok("Int") == assert_infer("4 % 1")
  assert Ok("Bool") == assert_infer("4 > 1")
  assert Ok("Bool") == assert_infer("4 >= 1")
  assert Ok("Bool") == assert_infer("4 <= 1")
  assert Ok("Bool") == assert_infer("4 < 1")

  assert Ok("Int") == assert_infer("1000_000")
  assert Ok("Int") == assert_infer("1_000")
  assert Ok("Float") == assert_infer("1_000.")
  assert Ok("Float") == assert_infer("10_000.001")
  assert Ok("Float") == assert_infer("100_000.")

  assert Ok("Nil") == assert_infer("Nil")

  assert Ok("a") == assert_infer("todo")
  assert Ok("Bool") == assert_infer("1 == todo")
  assert Ok("Bool") == assert_infer("todo != 1")
  assert Ok("Int") == assert_infer("todo + 1")
  assert Ok("Int") == assert_infer("todo(\"test\") + 1")

  assert Ok("Int") == assert_infer("0xF")
  assert Ok("Int") == assert_infer("0o11")
  assert Ok("Int") == assert_infer("0b1010")

  assert Ok("Float") == assert_infer("6.02e23")
  assert Ok("Float") == assert_infer("6.02e-23")
}

pub fn assert_patterns_test() {
  assert Ok("Int") == assert_infer("let assert [] = [] 1")
  assert Ok("Int") == assert_infer("let assert [a] = [1] a")
  assert Ok("Int") == assert_infer("let assert [a, 2] = [1] a")
  assert Ok("Int") == assert_infer("let assert [a, .._] = [1] a")
  assert Ok("fn(List(a)) -> a")
    == assert_infer("fn(x) { let assert [a] = x a }")
  assert Ok("fn(List(Int)) -> Int")
    == assert_infer("fn(x) { let assert [a] = x a + 1 }")
  assert Ok("Float") == assert_infer("let assert _x = 1 2.0")
  assert Ok("Float") == assert_infer("let assert _ = 1 2.0")
  assert Ok("Int") == assert_infer("let assert #(tag, x) = #(1.0, 1) x")
  assert Ok("fn(#(a, b)) -> a")
    == assert_infer("fn(x) { let assert #(a, b) = x a }")
  assert Ok("Int") == assert_infer("let assert 5: Int = 5 5")
}

pub fn lists_test() {
  assert Ok("List(a)") == assert_infer("[]")
  assert Ok("List(Int)") == assert_infer("[1]")
  assert Ok("List(Int)") == assert_infer("[1, 2, 3]")
  assert Ok("List(List(a))") == assert_infer("[[]]")
  assert Ok("List(List(Float))") == assert_infer("[[1.0, 2.0]]")
  assert Ok("List(fn(a) -> a)") == assert_infer("[fn(x) { x }]")
  assert Ok("List(fn(Int) -> Int)") == assert_infer("[fn(x) { x + 1 }]")
  assert Ok("List(fn(Int) -> Int)")
    == assert_infer("[fn(x) { x }, fn(x) { x + 1 }]")
  assert Ok("List(fn(Int) -> Int)")
    == assert_infer("[fn(x) { x + 1 }, fn(x) { x }]")
  assert Ok("List(List(a))") == assert_infer("[[], []]")
  assert Ok("List(List(Int))") == assert_infer("[[], [1]]")
  assert Ok("List(Int)") == assert_infer("[1, ..[2, ..[]]]")
  assert Ok("List(fn(a) -> a)") == assert_infer("[fn(x) { x }, ..[]]")
  assert Ok("List(Int)") == assert_infer("let x = [1, ..[]] [2, ..x]")
}

pub fn trailing_comma_lists_test() {
  assert Ok("List(fn(a) -> a)") == assert_infer("[fn(x) { x },..[]]")
  assert Ok("List(fn(a) -> a)") == assert_infer("let f = fn(x) { x } [f, f]")
  assert Ok("List(#(List(a), List(b)))") == assert_infer("[#([], [])]")
}

pub fn tuples_test() {
  assert Ok("#(Int)") == assert_infer("#(1)")
  assert Ok("#(Int, Float)") == assert_infer("#(1, 2.0)")
  assert Ok("#(Int, Float, Int)") == assert_infer("#(1, 2.0, 3)")
  assert Ok("#(Int, Float, #(Int, Int))") == assert_infer("#(1, 2.0, #(1, 1))")
}

pub fn expr_fn_test() {
  assert Ok("fn(a) -> a") == assert_infer("fn(x) { x }")
  assert Ok("fn(a) -> a") == assert_infer("fn(x) { x }")
  assert Ok("fn(a, b) -> a") == assert_infer("fn(x, y) { x }")
  assert Ok("fn(a, b) -> List(c)") == assert_infer("fn(x, y) { [] }")
  assert Ok("Int") == assert_infer("let x = 1.0 1")
  assert Ok("Int") == assert_infer("let id = fn(x) { x } id(1)")
  assert Ok("Float") == assert_infer("let x = fn() { 1.0 } x()")
  assert Ok("Int") == assert_infer("fn(x) { x }(1)")
  assert Ok("fn() -> Int") == assert_infer("fn() { 1 }")
  assert Ok("fn() -> Float") == assert_infer("fn() { 1.1 }")
  assert Ok("fn(a) -> Float") == assert_infer("fn(x) { 1.1 }")
  assert Ok("fn(a) -> a") == assert_infer("fn(x) { x }")
  assert Ok("fn(a) -> Float") == assert_infer("let x = fn(x) { 1.1 } x")
  assert Ok("fn(a, b, c) -> Int") == assert_infer("fn(x, y, z) { 1 }")
  assert Ok("fn(a) -> a") == assert_infer("fn(x) { let y = x y }")
  assert Ok("Int") == assert_infer("let id = fn(x) { x } id(1)")
  assert Ok("Int")
    == assert_infer(
      "let constant = fn(x) { fn(y) { x } } let one = constant(1) one(2.0)",
    )

  assert Ok("fn(fn(Int) -> a) -> a") == assert_infer("fn(f) { f(1) }")
  assert Ok("fn(fn(a) -> b, a) -> b") == assert_infer("fn(f, x) { f(x) }")
  assert Ok("fn(fn(a) -> b) -> fn(a) -> b")
    == assert_infer("fn(f) { fn(x) { f(x) } }")
  assert Ok("fn(fn(a, b) -> c) -> fn(a) -> fn(b) -> c")
    == assert_infer("fn(f) { fn(x) { fn(y) { f(x, y) } } }")
  assert Ok("fn(fn(a) -> fn(b) -> c) -> fn(a, b) -> c")
    == assert_infer("fn(f) { fn(x, y) { f(x)(y) } }")
  assert Ok("fn(fn(a) -> b) -> fn(a) -> b")
    == assert_infer("fn(f) { fn(x) { let ff = f ff(x) } }")
  assert Ok("fn(fn(a) -> fn(b) -> c) -> fn(a, b) -> c")
    == assert_infer("fn(f) { fn(x, y) { let ff = f(x) ff(y) } }")
  assert Ok("fn(a) -> fn(b) -> a") == assert_infer("fn(x) { fn(y) { x } }")
  assert Ok("fn(fn() -> a) -> a") == assert_infer("fn(f) { f() }")
  assert Ok("fn(fn(a) -> a, a) -> a") == assert_infer("fn(f, x) { f(f(x)) }")
  assert Ok("fn(fn(fn(a) -> a) -> b) -> b")
    == assert_infer("let id = fn(a) { a } fn(x) { x(id) }")

  assert Ok("fn(Int) -> Int")
    == assert_infer("let add = fn(x, y) { x + y } add(_, 2)")
  assert Ok("fn(a) -> #(Int, a)") == assert_infer("fn(x) { #(1, x) }")
  assert Ok("fn(a, b) -> #(a, b)") == assert_infer("fn(x, y) { #(x, y) }")
  assert Ok("fn(a) -> #(a, a)") == assert_infer("fn(x) { #(x, x) }")
  assert Ok("fn(Int) -> Int") == assert_infer("fn(x) -> Int { x }")
  assert Ok("fn(a) -> a") == assert_infer("fn(x) -> a { x }")
  assert Ok("fn() -> Int") == assert_infer("fn() -> Int { 2 }")
}

pub fn case_test() {
  assert Ok("Int") == assert_infer("case 1 { a -> 1 }")
  assert Ok("Float") == assert_infer("case 1 { a -> 1.0 b -> 2.0 c -> 3.0 }")
  assert Ok("Int") == assert_infer("case 1 { a -> a }")
  assert Ok("Int") == assert_infer("case 1 { 1 -> 10 2 -> 20 x -> x * 10 }")
  assert Ok("Int") == assert_infer("case 2.0 { 2.0 -> 1 x -> 0 }")
  assert Ok("Int") == assert_infer("case \"ok\" { \"ko\" -> 1 x -> 0 }")
}

pub fn multiple_subject_case_test() {
  assert Ok("Int") == assert_infer("case 1, 2.0 { a, b -> a }")
  assert Ok("Float") == assert_infer("case 1, 2.0 { a, b -> b }")
  assert Ok("Int") == assert_infer("case 1, 2.0, 3 { a, b, c -> a + c }")
}

pub fn tuple_index_test() {
  assert Ok("Int") == assert_infer("#(1, 2.0).0")
  assert Ok("Float") == assert_infer("#(1, 2.0).1")
}

pub fn pipe_test() {
  assert Ok("Int") == assert_infer("1 |> fn(x) { x }")
  assert Ok("Float") == assert_infer("1.0 |> fn(x) { x }")
  assert Ok("Int") == assert_infer("let id = fn(x) { x } 1 |> id")
  assert Ok("Float") == assert_infer("let id = fn(x) { x } 1.0 |> id")
  assert Ok("Int")
    == assert_infer("let add = fn(x, y) { x + y } 1 |> add(_, 2)")
  assert Ok("Int")
    == assert_infer("let add = fn(x, y) { x + y } 1 |> add(2, _)")
  assert Ok("Int") == assert_infer("let add = fn(x, y) { x + y } 1 |> add(2)")
  assert Ok("Int") == assert_infer("let id = fn(x) { x } 1 |> id()")
  assert Ok("Int")
    == assert_infer("let add = fn(x) { fn(y) { y + x } } 1 |> add(1)")
  assert Ok("Int")
    == assert_infer(
      "let add = fn(x, _, _) { fn(y) { y + x } } 1 |> add(1, 2, 3)",
    )
}

pub fn bit_array_test() {
  assert Ok("Int") == assert_infer("let assert <<x>> = <<1>> x")
}

pub fn bit_array2_test() {
  assert Ok("Int") == assert_infer("let assert <<x>> = <<1>> x")
}

pub fn bit_array3_test() {
  assert Ok("Float") == assert_infer("let assert <<x:float>> = <<1>> x")
}

pub fn bit_array4_test() {
  assert Ok("BitArray") == assert_infer("let assert <<x:bytes>> = <<1>> x")
}

pub fn bit_array5_test() {
  assert Ok("BitArray") == assert_infer("let assert <<x:bytes>> = <<1>> x")
}

pub fn bit_array6_test() {
  assert Ok("BitArray") == assert_infer("let assert <<x:bits>> = <<1>> x")
}

pub fn bit_array7_test() {
  assert Ok("BitArray") == assert_infer("let assert <<x:bits>> = <<1>> x")
}

pub fn bit_array8_test() {
  assert Ok("UtfCodepoint")
    == assert_infer("let assert <<x:utf8_codepoint>> = <<128013:32>> x")
}

pub fn bit_array9_test() {
  assert Ok("UtfCodepoint")
    == assert_infer("let assert <<x:utf16_codepoint>> = <<128013:32>> x")
}

pub fn bit_array10_test() {
  assert Ok("UtfCodepoint")
    == assert_infer("let assert <<x:utf32_codepoint>> = <<128013:32>> x")
}

pub fn bit_array11_test() {
  assert Ok("BitArray")
    == assert_infer("let a = <<1>> let assert <<x:bits>> = <<1, a:2-bits>> x")
}

pub fn bit_array12_test() {
  assert Ok("BitArray") == assert_infer("let x = <<<<1>>:bits, <<2>>:bits>> x")
}

pub fn let_as_expression_test() {
  assert Ok("Int") == assert_infer("let x = 1")
}

pub fn let_as_expression1_test() {
  assert Ok("Int") == assert_infer("let x = { let x = 1 }")
}

pub fn let_as_expression2_test() {
  assert Ok("Float") == assert_infer("let x = { let x = 1. }")
}

pub fn string_concat_ok_test() {
  assert Ok("String") == assert_infer(" \"1\" <> \"2\" ")
}

fn assert_infer(code: String) -> Result(String, gloat.TypeError) {
  let module_code = "const top = {\n" <> code <> "\n}"
  process(module_code, "top")
}

fn process(code, name) {
  result.map(infer_scheme_from_glance(code, name), gloat.scheme_to_string_gleam)
}

fn infer_scheme_from_glance(
  code: String,
  name: String,
) -> Result(gloat.Scheme, gloat.TypeError) {
  let assert Ok(parsed) = glance.module(code)
  result.try(
    gloat.add_module_with_target(gloat.builtin_env(), parsed, "erlang"),
    fn(env_) {
      case env.resolve(env_, name) {
        Ok(scheme) -> Ok(scheme)
        Error(_) ->
          Error(type_error.new(
            "definition not found in env",
            types.unknown_span,
          ))
      }
    },
  )
}
