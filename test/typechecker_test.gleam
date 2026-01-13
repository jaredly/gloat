import glance
import gleam/list
import gleam/option.{None}
import gleeunit
import typechecker
import typechecker/ast
import typechecker/env
import typechecker/types

type Lol {
  Lol(name: Int)
}

fn ho(n: Lol) {
  1
}

fn hi(v) {
  ho(v)
  v.name
}

pub fn main() -> Nil {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn infer_int_test() {
  let loc = 0
  let expr = ast.Eprim(ast.Pint(1, loc), loc)
  let inferred = typechecker.infer_expr(typechecker.builtin_env(), expr)
  assert types.type_eq(inferred, types.Tcon("int", loc))
}

pub fn infer_lambda_identity_test() {
  let loc = 0
  let expr = ast.Elambda([ast.Pvar("x", loc)], ast.Evar("x", loc), loc)
  let inferred = typechecker.infer_expr(typechecker.builtin_env(), expr)
  assert typechecker.type_to_string(inferred) == "(fn [x:0] x:0)"
}

pub fn infer_float_test() {
  let loc = 0
  let expr = ast.Eprim(ast.Pfloat(1.5, loc), loc)
  let inferred = typechecker.infer_expr(typechecker.builtin_env(), expr)
  assert types.type_eq(inferred, types.Tcon("float", loc))
}

pub fn infer_list_pattern_test() {
  let loc = 0
  let pat = ast.Plist([ast.Pvar("x", loc), ast.Pvar("y", loc)], None, loc)
  let expr = ast.Elambda([pat], ast.Evar("x", loc), loc)
  let inferred = typechecker.infer_expr(typechecker.builtin_env(), expr)
  assert typechecker.type_to_string(inferred)
    == "(fn [(list list_item:0)] list_item:0)"
}

pub fn infer_alias_pattern_test() {
  let loc = 0
  let pat = ast.Pas("y", ast.Pvar("x", loc), loc)
  let expr = ast.Elambda([pat], ast.Evar("y", loc), loc)
  let inferred = typechecker.infer_expr(typechecker.builtin_env(), expr)
  assert typechecker.type_to_string(inferred) == "(fn [x:0] x:0)"
}

pub fn infer_tuple_index_test() {
  let loc = 0
  let tuple =
    ast.Etuple(
      [
        ast.Eprim(ast.Pint(1, loc), loc),
        ast.Eprim(ast.Pbool(True, loc), loc),
      ],
      loc,
    )
  let expr = ast.EtupleIndex(tuple, 1, loc)
  let inferred = typechecker.infer_expr(typechecker.builtin_env(), expr)
  assert types.type_eq(inferred, types.Tcon("bool", loc))
}

pub fn infer_let_test() {
  let loc = 0
  let id_expr = ast.Elambda([ast.Pvar("x", loc)], ast.Evar("x", loc), loc)
  let body =
    ast.Eapp(ast.Evar("id", loc), [ast.Eprim(ast.Pint(2, loc), loc)], loc)
  let expr = ast.Elet([#(ast.Pvar("id", loc), id_expr)], body, loc)
  let inferred = typechecker.infer_expr(typechecker.builtin_env(), expr)
  assert types.type_eq(inferred, types.Tcon("int", loc))
}

pub fn infer_match_test() {
  let loc = 0
  let target = ast.Eprim(ast.Pint(1, loc), loc)
  let case1 = #(
    ast.Pprim(ast.Pint(1, loc), loc),
    None,
    ast.Eprim(ast.Pbool(True, loc), loc),
  )
  let case2 = #(ast.Pany(loc), None, ast.Eprim(ast.Pbool(False, loc), loc))
  let expr = ast.Ematch(target, [case1, case2], loc)
  let inferred = typechecker.infer_expr(typechecker.builtin_env(), expr)
  assert types.type_eq(inferred, types.Tcon("bool", loc))
}

pub fn glance_module_to_tops_test() {
  let code =
    "
pub fn id(x) { x }
const one = 1
"
  let assert Ok(parsed) = glance.module(code)
  case typechecker.from_glance_module(parsed) {
    Ok(tops) -> {
      assert list.length(tops) == 2
      assert list.any(tops, fn(top) {
        case top {
          ast.Tdef("id", _, _, _) -> True
          _ -> False
        }
      })
      assert list.any(tops, fn(top) {
        case top {
          ast.Tdef("one", _, _, _) -> True
          _ -> False
        }
      })
    }
    Error(_) -> panic as "expected successful conversion"
  }
}

pub fn infer_from_glance_const_test() {
  let code =
    "
const answer = 42
"
  let scheme = infer_scheme_from_glance(code, "answer")
  assert typechecker.scheme_to_string(scheme) == "int"
}

pub fn infer_from_glance_identity_test() {
  let code =
    "
pub fn id(x) { x }
"
  let scheme = infer_scheme_from_glance(code, "id")
  assert typechecker.scheme_to_string(scheme) == "forall x:1 : (fn [x:1] x:1)"
}

pub fn infer_from_glance_pair_test() {
  let code =
    "
pub fn pair(a, b) { #(a, b) }
"
  let scheme = infer_scheme_from_glance(code, "pair")
  assert typechecker.scheme_to_string(scheme)
    == "forall a:1 b:2 : (fn [a:1 b:2] (, a:1 b:2))"
}

pub fn infer_with_binop_test() {
  let code = "fn even(x) { x + 3 }"
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

pub fn case_guard_from_glance_test() {
  let code =
    "
const top = case 1 {
  x if x > 0 -> x
  _ -> 0
}
"
  assert process(code, "top") == "int"
}

pub fn concat_pattern_from_glance_test() {
  let code =
    "
const top = case \"ok\" {
  \"o\" <> rest -> rest
  _ -> \"\"
}
"
  assert process(code, "top") == "string"
}

pub fn echo_from_glance_test() {
  let code = "const top = echo 1"
  assert process(code, "top") == "int"
}

pub fn record_constructor_from_glance_test() {
  let code =
    "
type User { User(name: String, age: Int) }
const top = User(name: \"A\", age: 1)
"
  assert process(code, "top") == "User"
}

pub fn record_constructor_positional_from_glance_test() {
  let code =
    "
type User { User(name: String, age: Int) }
const top = User(\"A\", 1)
"
  assert process(code, "top") == "User"
}

pub fn record_field_access_from_glance_test() {
  let code =
    "
type User { User(name: String, age: Int) }
const user = User(name: \"A\", age: 1)
const top = user.age
"
  assert process(code, "top") == "int"
}

pub fn record_update_from_glance_test() {
  let code =
    "
type User { User(name: String, age: Int) }
const user = User(name: \"A\", age: 1)
const top = User(..user, age: 2)
"
  assert process(code, "top") == "User"
}

fn process(code, name) {
  typechecker.scheme_to_string(infer_scheme_from_glance(code, name))
}

pub fn glance_binop_conversion_test() {
  let code =
    "
fn even(x) {
  x + 3
}
"
  let assert Ok(parsed) = glance.module(code)
  let tops = case typechecker.from_glance_module(parsed) {
    Ok(tops) -> tops
    Error(_) -> panic as "failed to convert glance module"
  }
  let assert [ast.Tdef("even", _, expr, _)] = tops
  let assert ast.Elambda(_, body, _) = expr
  case body {
    ast.Eapp(
      ast.Eapp(ast.Evar("+", _), [ast.Evar("x", _)], _),
      [ast.Eprim(ast.Pint(3, _), _)],
      _,
    ) -> Nil
    _ -> panic as "unexpected binop conversion shape"
  }
}

pub fn infer_binop_ast_test() {
  let loc = 0
  let body =
    ast.Eapp(
      ast.Eapp(ast.Evar("+", loc), [ast.Evar("x", loc)], loc),
      [ast.Eprim(ast.Pint(3, loc), loc)],
      loc,
    )
  let expr = ast.Elambda([ast.Pvar("x", loc)], body, loc)
  let inferred = typechecker.infer_expr(typechecker.builtin_env(), expr)
  assert typechecker.type_to_string(inferred) == "(fn [int] int)"
}

fn infer_scheme_from_glance(code: String, name: String) -> typechecker.Scheme {
  let assert Ok(parsed) = glance.module(code)
  let tops = case typechecker.from_glance_module(parsed) {
    Ok(tops) -> tops
    Error(_) -> panic as "failed to convert glance module"
  }
  let env_ = typechecker.add_stmts(typechecker.builtin_env(), tops)
  case env.resolve(env_, name) {
    Ok(scheme) -> scheme
    Error(_) -> panic as "definition not found in env"
  }
}
