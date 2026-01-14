import glance
import gleeunit
import test_panic
import typechecker
import typechecker/env

pub fn main() -> Nil {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn infer_int_test() {
  let code = "const top = 1"
  assert process(code, "top") == "int"
}

pub fn infer_float_test() {
  let code = "const top = 1.5"
  assert process(code, "top") == "float"
}

pub fn infer_block_let_test() {
  let code = "const top = { let id = fn(x) { x } id(2) }"
  assert process(code, "top") == "int"
}

pub fn infer_case_list_pattern_test() {
  let code = "const top = case [1, 2] { [x, _] -> x }"
  assert process(code, "top") == "int"
}

pub fn infer_from_glance_const_test() {
  let code = "\nconst answer = 42\n"
  let scheme = infer_scheme_from_glance(code, "answer")
  assert typechecker.scheme_to_string(scheme) == "int"
}

pub fn infer_from_glance_identity_test() {
  let code = "\npub fn id(x) { x }\n"
  let scheme = infer_scheme_from_glance(code, "id")
  assert typechecker.scheme_to_string(scheme) == "forall x:1 : (fn [x:1] x:1)"
}

pub fn infer_from_glance_pair_test() {
  let code = "\npub fn pair(a, b) { #(a, b) }\n"
  let scheme = infer_scheme_from_glance(code, "pair")
  assert typechecker.scheme_to_string(scheme)
    == "forall a:1 b:2 : (fn [a:1 b:2] (, a:1 b:2))"
}

pub fn infer_with_binop_test() {
  let code = "fn even(x) { x + 3 }"
  assert process(code, "even") == "(fn [int] int)"
}

pub fn mutual_recursion_test() {
  let code = "fn even(x) { x + odd(x) }\nfn odd(v) { v - 1 }"
  assert process(code, "even") == "(fn [int] int)"
}

pub fn mutual_recursion_oorder_test() {
  let code = "fn even(x) { x + odd(x) }\nconst x = \"hi\"\nfn odd(v) { v - 1 }"
  assert process(code, "even") == "(fn [int] int)"
}

pub fn id_id_test() {
  let code = "fn id(x) { x }\nconst top = id(id)"
  assert process(code, "top") == "forall x:1:3 : (fn [x:1:3] x:1:3)"
}

pub fn tuple_test() {
  let code = "const top = #(1, 2)"
  assert process(code, "top") == "(, int int)"
}

pub fn tuple3_test() {
  let code = "const top = #(1, 2, \"hi\")"
  assert process(code, "top") == "(, int int string)"
}

pub fn tuple_index_from_glance_test() {
  let code = "const top = #(1, 2, 3).1"
  assert process(code, "top") == "int"
}

pub fn list_from_glance_test() {
  let code = "const top = [1, 2]"
  assert process(code, "top") == "(list int)"
}

pub fn list_spread_from_glance_test() {
  let code = "const xs = [1]\nconst top = [..xs]"
  assert process(code, "top") == "(list int)"
}

pub fn list_spread_tail_from_glance_test() {
  let code = "const xs = [1]\nconst top = [1, 2, ..xs]"
  assert process(code, "top") == "(list int)"
}

pub fn bitstring_from_glance_test() {
  let code = "const top = <<1, 2>>"
  assert process(code, "top") == "bitstring"
}

pub fn fn_capture_from_glance_test() {
  let code = "fn sum3(a, b, c) { a + b + c }\nconst top = sum3(1, _, 3)"
  assert process(code, "top") == "(fn [int] int)"
}

pub fn use_statement_test() {
  let code = "fn with(x, f) { f(x) }\nconst top = { use y <- with(1)\n y }"
  assert process(code, "top") == "int"
}

pub fn use_statement_multiple_patterns_test() {
  let code =
    "fn with2(x, y, f) { f(x, y) }\nconst top = { use a, b <- with2(1, 2)\n a + b }"
  assert process(code, "top") == "int"
}

pub fn use_statement_annotation_test() {
  let code = "fn with(x, f) { f(x) }\nconst top = { use y: Int <- with(1)\n y }"
  assert process(code, "top") == "int"
}

pub fn use_statement_direct_function_test() {
  let code = "fn wrap(f) { f(1) }\nconst top = { use y <- wrap\n y }"
  assert process(code, "top") == "int"
}

pub fn use_statement_tuple_pattern_test() {
  let code =
    "fn with_pair(f) { f(#(1, 2)) }\nconst top = { use #(a, b) <- with_pair\n a + b }"
  assert process(code, "top") == "int"
}

pub fn use_statement_list_pattern_test() {
  let code =
    "fn with_list(f) { f([1, 2, 3]) }\nconst top = { use [a, b, .._] <- with_list\n a + b }"
  assert process(code, "top") == "int"
}

pub fn use_statement_bitstring_pattern_test() {
  let code =
    "fn with_bits(f) { f(<<1, 2>>) }\nconst top = { use <<a, b>> <- with_bits\n a + b }"
  assert process(code, "top") == "int"
}

pub fn labelled_function_args_test() {
  let code = "fn add(a, b) { a + b }\nconst top = add(b: 2, a: 1)"
  assert process(code, "top") == "int"
}

pub fn labelled_function_args_mixed_test() {
  let code = "fn add(a, b, c) { a + b + c }\nconst top = add(1, c: 3, b: 2)"
  assert process(code, "top") == "int"
}

pub fn labelled_function_args_shorthand_test() {
  let code =
    "fn add(a, b) { a + b }\nconst a = 1\nconst b = 2\nconst top = add(a:, b:)"
  assert process(code, "top") == "int"
}

pub fn labelled_constructor_args_test() {
  let code =
    "type User { User(name: String, age: Int) }\nconst top = User(age: 1, name: \"A\")"
  assert process(code, "top") == "User"
}

pub fn labelled_constructor_args_shorthand_test() {
  let code =
    "type User { User(name: String, age: Int) }\nconst name = \"A\"\nconst age = 1\nconst top = User(name:, age:)"
  assert process(code, "top") == "User"
}

pub fn labelled_constructor_missing_field_error_test() {
  let code =
    "type User { User(name: String, age: Int) }\nconst top = User(name: \"A\")"
  assert test_panic.catches_panic(fn() { process(code, "top") })
}

pub fn labelled_constructor_unknown_field_error_test() {
  let code =
    "type User { User(name: String, age: Int) }\nconst top = User(foo: \"A\", age: 1)"
  assert test_panic.catches_panic(fn() { process(code, "top") })
}

pub fn use_tuple_pattern_arity_error_test() {
  let code =
    "fn with_pair(f) { f(#(1, 2)) }\nconst top = { use #(a, b, c) <- with_pair\n a }"
  assert test_panic.catches_panic(fn() { process(code, "top") })
}

pub fn labelled_function_args_type_error_test() {
  let code = "fn add(a, b) { a + b }\nconst top = add(a: \"x\", b: 1)"
  assert test_panic.catches_panic(fn() { process(code, "top") })
}

pub fn case_guard_from_glance_test() {
  let code = "\nconst top = case 1 {\n  x if x > 0 -> x\n  _ -> 0\n}\n"
  assert process(code, "top") == "int"
}

pub fn concat_pattern_from_glance_test() {
  let code =
    "\nconst top = case \"ok\" {\n  \"o\" <> rest -> rest\n  _ -> \"\"\n}\n"
  assert process(code, "top") == "string"
}

pub fn echo_from_glance_test() {
  let code = "const top = echo 1"
  assert process(code, "top") == "int"
}

pub fn record_constructor_from_glance_test() {
  let code =
    "\ntype User { User(name: String, age: Int) }\nconst top = User(name: \"A\", age: 1)\n"
  assert process(code, "top") == "User"
}

pub fn record_constructor_positional_from_glance_test() {
  let code =
    "\ntype User { User(name: String, age: Int) }\nconst top = User(\"A\", 1)\n"
  assert process(code, "top") == "User"
}

pub fn record_field_access_from_glance_test() {
  let code =
    "\ntype User { User(name: String, age: Int) }\nconst user = User(name: \"A\", age: 1)\nconst top = user.age\n"
  assert process(code, "top") == "int"
}

pub fn record_update_from_glance_test() {
  let code =
    "\ntype User { User(name: String, age: Int) }\nconst user = User(name: \"A\", age: 1)\nconst top = User(..user, age: 2)\n"
  assert process(code, "top") == "User"
}

fn process(code, name) {
  typechecker.scheme_to_string(infer_scheme_from_glance(code, name))
}

fn infer_scheme_from_glance(code: String, name: String) -> typechecker.Scheme {
  let assert Ok(parsed) = glance.module(code)
  let env_ = typechecker.add_module(typechecker.builtin_env(), parsed)
  case env.resolve(env_, name) {
    Ok(scheme) -> scheme
    Error(_) -> panic as "definition not found in env"
  }
}
