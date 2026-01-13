import gleeunit
import typechecker
import typechecker/ast
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
