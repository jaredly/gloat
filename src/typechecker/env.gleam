import gleam/dict
import gleam/list
import gleam/option
import gleam/set
import typechecker/scheme
import typechecker/types

pub type TEnv {
  TEnv(
    values: dict.Dict(String, scheme.Scheme),
    tcons: dict.Dict(
      String,
      #(List(String), List(#(option.Option(String), types.Type)), types.Type),
    ),
    types: dict.Dict(String, #(Int, set.Set(String))),
    aliases: dict.Dict(String, #(List(String), types.Type)),
    modules: dict.Dict(String, String),
  )
}

pub fn empty() -> TEnv {
  TEnv(dict.new(), dict.new(), dict.new(), dict.new(), dict.new())
}

pub fn merge(one: TEnv, two: TEnv) -> TEnv {
  let TEnv(v1, c1, t1, a1, m1) = one
  let TEnv(v2, c2, t2, a2, m2) = two
  TEnv(
    dict.merge(v1, v2),
    dict.merge(c1, c2),
    dict.merge(t1, t2),
    dict.merge(a1, a2),
    dict.merge(m1, m2),
  )
}

pub fn with_type(env: TEnv, name: String, scheme_: scheme.Scheme) -> TEnv {
  let TEnv(values, tcons, types_, aliases, modules) = env
  TEnv(dict.insert(values, name, scheme_), tcons, types_, aliases, modules)
}

pub fn with_scope(env: TEnv, scope: dict.Dict(String, scheme.Scheme)) -> TEnv {
  let TEnv(values, tcons, types_, aliases, modules) = env
  TEnv(dict.merge(values, scope), tcons, types_, aliases, modules)
}

pub fn resolve(env: TEnv, name: String) -> Result(scheme.Scheme, Nil) {
  let TEnv(values, _, _, _, _) = env
  dict.get(values, name)
}

pub fn with_module(env: TEnv, alias: String, module_name: String) -> TEnv {
  let TEnv(values, tcons, types_, aliases, modules) = env
  TEnv(values, tcons, types_, aliases, dict.insert(modules, alias, module_name))
}

pub fn resolve_module(env: TEnv, alias: String) -> Result(String, Nil) {
  let TEnv(_values, _tcons, _types, _aliases, modules) = env
  dict.get(modules, alias)
}

pub fn type_free(env: TEnv) -> set.Set(String) {
  let TEnv(values, _, _, _, _) = env
  values
  |> dict.values
  |> list.map(scheme.scheme_free)
  |> list.fold(set.new(), set.union)
}

pub fn apply(subst: types.Subst, env: TEnv) -> TEnv {
  let TEnv(values, tcons, types_, aliases, modules) = env
  TEnv(scope_apply(subst, values), tcons, types_, aliases, modules)
}

pub fn scope_apply(
  subst: types.Subst,
  scope: dict.Dict(String, scheme.Scheme),
) -> dict.Dict(String, scheme.Scheme) {
  dict.map_values(scope, fn(_k, v) { scheme.scheme_apply(subst, v) })
}

pub fn generalize(env: TEnv, type_: types.Type) -> scheme.Scheme {
  scheme.Forall(set.difference(types.type_free(type_), type_free(env)), type_)
}
