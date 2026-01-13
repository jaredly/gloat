import gleam/dict
import gleam/list
import gleam/set
import gleam/string
import typechecker/runtime

pub type Type {
  Tvar(String, Int)
  Tapp(Type, Type, Int)
  Tcon(String, Int)
}

pub type Subst = dict.Dict(String, Type)

pub fn type_eq(one: Type, two: Type) -> Bool {
  case #(one, two) {
    #(Tvar(id, _), Tvar(id2, _)) -> id == id2
    #(Tapp(t1, a1, _), Tapp(t2, a2, _)) ->
      type_eq(t1, t2) && type_eq(a1, a2)
    #(Tcon(name, _), Tcon(name2, _)) -> name == name2
    _ -> False
  }
}

pub fn tfn(arg: Type, body: Type, loc: Int) -> Type {
  Tapp(Tapp(Tcon("->", loc), arg, loc), body, loc)
}

pub fn tfns(args: List(Type), body: Type, loc: Int) -> Type {
  list.fold_right(args, body, fn(arg, acc) { tfn(arg, acc, loc) })
}

pub const tint: Type = Tcon("int", -1)

pub fn type_to_string_simple(type_: Type) -> String {
  case type_ {
    Tvar(name, _) -> name
    Tapp(Tapp(Tcon("->", _), arg, _), res, _) ->
      "(fn [" <> type_to_string_simple(arg) <> "] " <> type_to_string_simple(res) <> ")"
    Tapp(target, arg, _) ->
      "(" <> type_to_string_simple(target) <> " " <> type_to_string_simple(arg) <> ")"
    Tcon(name, _) -> name
  }
}

pub fn type_to_string(type_: Type) -> String {
  case type_ {
    Tvar(name, _) -> name
    Tapp(Tapp(Tcon("->", _), _, _), _, _) -> {
      let #(args, result) = unwrap_fn(type_)
      let args_s = list.map(args, type_to_string)
      "(fn [" <> string.join(args_s, with: " ") <> "] " <> type_to_string(result) <> ")"
    }
    Tapp(_, _, _) -> {
      let #(target, args) = unwrap_app(type_, [])
      let args_s = list.map(args, type_to_string)
      "(" <> type_to_string(target) <> " " <> string.join(args_s, with: " ") <> ")"
    }
    Tcon(name, _) -> name
  }
}

pub fn unwrap_fn(type_: Type) -> #(List(Type), Type) {
  case type_ {
    Tapp(Tapp(Tcon("->", _), a, _), b, _) -> {
      let #(args, res) = unwrap_fn(b)
      #([a, ..args], res)
    }
    _ -> #([], type_)
  }
}

pub fn unwrap_app(type_: Type, args: List(Type)) -> #(Type, List(Type)) {
  case type_ {
    Tapp(a, b, _) -> unwrap_app(a, [b, ..args])
    _ -> #(type_, args)
  }
}

pub fn type_free(type_: Type) -> set.Set(String) {
  case type_ {
    Tvar(id, _) -> set.insert(set.new(), id)
    Tcon(_, _) -> set.new()
    Tapp(a, b, _) -> set.union(type_free(a), type_free(b))
  }
}

pub fn type_apply(subst: Subst, type_: Type) -> Type {
  case type_ {
    Tvar(id, _) ->
      case dict.get(subst, id) {
        Ok(t) -> t
        Error(_) -> type_
      }
    Tapp(target, arg, loc) ->
      Tapp(type_apply(subst, target), type_apply(subst, arg), loc)
    _ -> type_
  }
}

pub fn compose_subst(new_subst: Subst, old_subst: Subst) -> Subst {
  let applied_old = dict.map_values(old_subst, fn(_k, v) { type_apply(new_subst, v) })
  dict.merge(applied_old, new_subst)
}

pub fn type_con_to_var(vars: set.Set(String), type_: Type) -> Type {
  case type_ {
    Tvar(_, _) -> type_
    Tcon(name, loc) ->
      case set.contains(vars, name) {
        True -> Tvar(name, loc)
        False -> type_
      }
    Tapp(a, b, loc) -> Tapp(type_con_to_var(vars, a), type_con_to_var(vars, b), loc)
  }
}

pub fn type_unroll_app(type_: Type) -> #(Type, List(#(Type, Int))) {
  case type_ {
    Tapp(target, arg, loc) -> {
      let #(target_inner, inner) = type_unroll_app(target)
      #(target_inner, [#(arg, loc), ..inner])
    }
    _ -> #(type_, [])
  }
}

pub fn tcon_and_args(type_: Type, coll: List(Type), loc: Int) -> #(String, List(Type)) {
  case type_ {
    Tvar(_, _) -> runtime.fatal(
      "Type not resolved " <> string.from_int(loc) <> " " <> runtime.jsonify(type_) <> " " <> runtime.jsonify(coll)
    )
    Tcon(name, _) -> #(name, coll)
    Tapp(target, arg, _) -> tcon_and_args(target, [arg, ..coll], loc)
  }
}

pub fn type_resolve_aliases(aliases: dict.Dict(String, #(List(String), Type)), type_: Type) -> Type {
  let #(target, args) = type_unroll_app(type_)
  let resolved_args =
    list.map(list.reverse(args), fn(#(arg, _loc)) { type_resolve_aliases(aliases, arg) })

  case target {
    Tcon(name, loc) ->
      case dict.get(aliases, name) {
        Ok(#(free, alias_type)) -> {
          let subst = dict.from_list(list.zip(free, resolved_args))
          type_resolve_aliases(aliases, type_apply(subst, alias_type))
        }
        Error(_) -> list.fold(resolved_args, target, fn(acc, arg) { Tapp(acc, arg, loc) })
      }
    Tvar(_, loc) -> list.fold(resolved_args, target, fn(acc, arg) { Tapp(acc, arg, loc) })
    _ -> target
  }
}
