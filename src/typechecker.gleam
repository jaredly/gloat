import glance as g
import typechecker/builtins
import typechecker/env
import typechecker/infer
import typechecker/scheme
import typechecker/state
import typechecker/types

pub type Expr =
  g.Expression

pub type Pat =
  g.Pattern

pub type Module =
  g.Module

pub type Type =
  types.Type

pub type Scheme =
  scheme.Scheme

pub type TEnv =
  env.TEnv

pub fn builtin_env() -> env.TEnv {
  builtins.builtin_env()
}

pub fn add_module(tenv: env.TEnv, module: g.Module) -> env.TEnv {
  builtins.add_module(tenv, module)
}

pub fn infer_expr(tenv: env.TEnv, expr: g.Expression) -> types.Type {
  state.run_empty(infer.infer_expr(tenv, expr))
}

pub fn type_to_string(type_: types.Type) -> String {
  types.type_to_string(type_)
}

pub fn scheme_to_string(scheme_: scheme.Scheme) -> String {
  scheme.scheme_to_string(scheme_)
}
