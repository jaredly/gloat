import typechecker/ast
import typechecker/builtins
import typechecker/env
import typechecker/infer
import typechecker/scheme
import typechecker/state
import typechecker/types

pub type Loc = ast.Loc
pub type Prim = ast.Prim
pub type Top = ast.Top
pub type Expr = ast.Expr
pub type Pat = ast.Pat
pub type Type = types.Type
pub type Scheme = scheme.Scheme
pub type TEnv = env.TEnv

pub fn builtin_env() -> env.TEnv {
  builtins.builtin_env()
}

pub fn add_stmts(tenv: env.TEnv, stmts: List(ast.Top)) -> env.TEnv {
  builtins.add_stmts(tenv, stmts)
}

pub fn infer_expr(tenv: env.TEnv, expr: ast.Expr) -> types.Type {
  state.run_empty(infer.infer_expr(tenv, expr))
}

pub fn type_to_string(type_: types.Type) -> String {
  types.type_to_string(type_)
}

pub fn scheme_to_string(scheme_: scheme.Scheme) -> String {
  scheme.scheme_to_string(scheme_)
}
