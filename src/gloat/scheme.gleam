import gleam/dict
import gleam/set
import gleam/string
import gloat/types

pub type Scheme {
  Forall(set.Set(String), types.Type)
}

pub fn scheme_free(scheme: Scheme) -> set.Set(String) {
  let Forall(vbls, type_) = scheme
  set.difference(types.type_free(type_), vbls)
}

pub fn scheme_apply(subst: types.Subst, scheme: Scheme) -> Scheme {
  let Forall(vbls, type_) = scheme
  let filtered = map_without(subst, vbls)
  Forall(vbls, types.type_apply(filtered, type_))
}

pub fn scheme_to_string(scheme: Scheme) -> String {
  let Forall(vbls, type_) = scheme
  let vars = set.to_list(vbls)
  case vars {
    [] -> types.type_to_string(type_)
    _ ->
      "forall "
      <> string.join(vars, with: " ")
      <> " : "
      <> types.type_to_string(type_)
  }
}

pub fn scheme_to_string_gleam(scheme: Scheme) -> String {
  let Forall(_, type_) = scheme
  types.type_to_string_gleam(type_)
}

pub fn map_without(subst: types.Subst, keys: set.Set(String)) -> types.Subst {
  set.fold(keys, subst, fn(acc, key) { dict.delete(acc, key) })
}
