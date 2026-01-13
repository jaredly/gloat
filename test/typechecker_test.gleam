import glance
import gleam/list
import gleeunit
import typechecker
import typechecker/ast
import typechecker/env
import typechecker/glance as tc_glance
import typechecker/types

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
    ast.Eprim(ast.Pbool(True, loc), loc),
  )
  let case2 = #(ast.Pany(loc), ast.Eprim(ast.Pbool(False, loc), loc))
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

pub fn glance_module_unsupported_test() {
  let code =
    "
const xs = [1, 2]
"
  let assert Ok(parsed) = glance.module(code)
  case typechecker.from_glance_module(parsed) {
    Ok(_) -> panic as "expected unsupported conversion error"
    Error(tc_glance.Unsupported(_)) -> Nil
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
    == "forall b:2 b:6 result:3 : (fn [(fn [(, (fn [b:2] result:3) b:6)] b:6) b:2] result:3)"
}

pub fn infer_with_binop_test() {
  let code =
    "
fn even(x) {
    x + 3
}
"
  let scheme = infer_scheme_from_glance(code, "even")
  assert typechecker.scheme_to_string(scheme) == "(fn [int] int)"
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
