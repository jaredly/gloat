import gleam/dict
import gleam/list
import gleam/option
import gleam/set
import gloat/scheme
import gloat/types

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
    params: dict.Dict(String, List(option.Option(String))),
    type_names: dict.Dict(String, String),
  )
}

pub fn empty() -> TEnv {
  TEnv(
    dict.new(),
    dict.new(),
    dict.new(),
    dict.new(),
    dict.new(),
    dict.new(),
    dict.new(),
  )
}

pub fn merge(one: TEnv, two: TEnv) -> TEnv {
  let TEnv(v1, c1, t1, a1, m1, p1, n1) = one
  let TEnv(v2, c2, t2, a2, m2, p2, n2) = two
  TEnv(
    dict.merge(v1, v2),
    dict.merge(c1, c2),
    dict.merge(t1, t2),
    dict.merge(a1, a2),
    dict.merge(m1, m2),
    dict.merge(p1, p2),
    dict.merge(n1, n2),
  )
}

pub fn with_type(env: TEnv, name: String, scheme_: scheme.Scheme) -> TEnv {
  let TEnv(values, tcons, types_, aliases, modules, params, type_names) = env
  TEnv(
    dict.insert(values, name, scheme_),
    tcons,
    types_,
    aliases,
    modules,
    params,
    type_names,
  )
}

pub fn with_type_params(
  env: TEnv,
  name: String,
  params_: List(option.Option(String)),
) -> TEnv {
  let TEnv(values, tcons, types_, aliases, modules, params, type_names) = env
  TEnv(
    values,
    tcons,
    types_,
    aliases,
    modules,
    dict.insert(params, name, params_),
    type_names,
  )
}

pub fn with_scope(env: TEnv, scope: dict.Dict(String, scheme.Scheme)) -> TEnv {
  let TEnv(values, tcons, types_, aliases, modules, params, type_names) = env
  TEnv(
    dict.merge(values, scope),
    tcons,
    types_,
    aliases,
    modules,
    params,
    type_names,
  )
}

pub fn resolve(env: TEnv, name: String) -> Result(scheme.Scheme, Nil) {
  let TEnv(values, tcons, _, _, _, _, _) = env
  case dict.get(values, name) {
    Ok(scheme_) -> Ok(scheme_)
    Error(_) ->
      case dict.get(tcons, name) {
        Ok(#(free, cargs, cres)) -> {
          let carg_types =
            list.map(cargs, fn(field) {
              let #(_label, t) = field
              t
            })
          Ok(scheme.Forall(
            set.from_list(free),
            types.tfns(carg_types, cres, types.unknown_span),
          ))
        }
        Error(_) -> Error(Nil)
      }
  }
}

pub fn resolve_params(
  env: TEnv,
  name: String,
) -> Result(List(option.Option(String)), Nil) {
  let TEnv(_values, tcons, _types, _aliases, _modules, params, _type_names) =
    env
  case dict.get(params, name) {
    Ok(params_) -> Ok(params_)
    Error(_) ->
      case dict.get(tcons, name) {
        Ok(#(_free, cargs, _cres)) ->
          Ok(
            list.map(cargs, fn(field) {
              let #(label, _t) = field
              label
            }),
          )
        Error(_) -> Error(Nil)
      }
  }
}

pub fn with_type_name(env: TEnv, name: String, qualified: String) -> TEnv {
  let TEnv(values, tcons, types_, aliases, modules, params, type_names) = env
  TEnv(
    values,
    tcons,
    types_,
    aliases,
    modules,
    params,
    dict.insert(type_names, name, qualified),
  )
}

pub fn resolve_type_name(env: TEnv, name: String) -> Result(String, Nil) {
  let TEnv(_values, _tcons, _types, _aliases, _modules, _params, type_names) =
    env
  dict.get(type_names, name)
}

pub fn type_exists(env: TEnv, name: String) -> Bool {
  let TEnv(_values, _tcons, types_, aliases, _modules, _params, _type_names) =
    env
  dict.has_key(types_, name) || dict.has_key(aliases, name)
}

pub fn with_module(env: TEnv, alias: String, module_name: String) -> TEnv {
  let TEnv(values, tcons, types_, aliases, modules, params, type_names) = env
  TEnv(
    values,
    tcons,
    types_,
    aliases,
    dict.insert(modules, alias, module_name),
    params,
    type_names,
  )
}

pub fn resolve_module(env: TEnv, alias: String) -> Result(String, Nil) {
  let TEnv(_values, _tcons, _types, _aliases, modules, _params, _type_names) =
    env
  dict.get(modules, alias)
}

pub fn type_free(env: TEnv) -> set.Set(String) {
  let TEnv(values, _, _, _, _, _, _) = env
  values
  |> dict.values
  |> list.map(scheme.scheme_free)
  |> list.fold(set.new(), set.union)
}

pub fn apply(subst: types.Subst, env: TEnv) -> TEnv {
  let TEnv(values, tcons, types_, aliases, modules, params, type_names) = env
  TEnv(
    scope_apply(subst, values),
    tcons,
    types_,
    aliases,
    modules,
    params,
    type_names,
  )
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
