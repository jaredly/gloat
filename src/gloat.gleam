import glance as g
import gleam/option
import gloat/builtins
import gloat/env
import gloat/glance as gloat_glance
import gloat/infer
import gloat/infer_state as is
import gloat/scheme
import gloat/type_error
import gloat/types

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

pub type TypeError =
  type_error.TypeError

pub fn builtin_env() -> env.TEnv {
  builtins.builtin_env()
}

pub fn add_module(
  tenv: env.TEnv,
  module: g.Module,
) -> Result(env.TEnv, type_error.TypeError) {
  builtins.add_module(tenv, module)
}

pub fn resolve(tenv: env.TEnv, name: String) -> option.Option(Scheme) {
  case env.resolve(tenv, name) {
    Ok(scheme) -> option.Some(scheme)
    _ -> option.None
  }
}

pub fn add_module_with_target(
  tenv: env.TEnv,
  module: g.Module,
  target: String,
) -> Result(env.TEnv, type_error.TypeError) {
  builtins.add_module(
    tenv,
    gloat_glance.filter_module_for_target(module, target),
  )
}

pub fn infer_expr(
  tenv: env.TEnv,
  expr: g.Expression,
) -> Result(types.Type, type_error.TypeError) {
  is.run_empty(infer.infer_expr(tenv, expr))
}

pub fn type_to_string_debug(type_: types.Type) -> String {
  types.type_to_string(type_)
}

pub fn type_to_string(type_: types.Type) -> String {
  types.type_to_string_gleam(type_)
}

pub fn scheme_to_string_debug(scheme_: scheme.Scheme) -> String {
  scheme.scheme_to_string(scheme_)
}

pub fn scheme_to_string(scheme_: scheme.Scheme) -> String {
  scheme.scheme_to_string_gleam(scheme_)
}
