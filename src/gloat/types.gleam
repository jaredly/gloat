import glance as g
import gleam/dict
import gleam/list
import gleam/set
import gleam/string
import gloat/runtime

pub type Span =
  g.Span

pub type Type {
  Tvar(String, Span)
  Tapp(Type, List(Type), Span)
  Tcon(String, Span)
  Ttuple(List(Type), Span)
  Tfn(List(Type), Type, Span)
}

pub type Subst =
  dict.Dict(String, Type)

pub fn type_eq(one: Type, two: Type) -> Bool {
  case one, two {
    Tvar(id, _), Tvar(id2, _) -> id == id2
    Tapp(t1, a1, _), Tapp(t2, a2, _) ->
      type_eq(t1, t2)
      && list.length(a1) == list.length(a2)
      && list.all(list.zip(a1, a2), fn(pair) {
        let #(left, right) = pair
        type_eq(left, right)
      })
    Tcon(name, _), Tcon(name2, _) -> name == name2
    Ttuple(args, _), Ttuple(args2, _) ->
      list.length(args) == list.length(args2)
      && list.all(list.zip(args, args2), fn(pair) {
        let #(left, right) = pair
        type_eq(left, right)
      })
    Tfn(args, res, _), Tfn(args2, res2, _) ->
      list.length(args) == list.length(args2)
      && list.all(list.zip(args, args2), fn(pair) {
        let #(left, right) = pair
        type_eq(left, right)
      })
      && type_eq(res, res2)
    _, _ -> False
  }
}

pub fn tfn(arg: Type, body: Type, span: Span) -> Type {
  Tfn([arg], body, span)
}

pub fn tfns(args: List(Type), body: Type, span: Span) -> Type {
  case args {
    [] -> body
    _ -> Tfn(args, body, span)
  }
}

pub const unknown_span: Span = g.Span(-1, -1)

pub const tint: Type = Tcon("Int", unknown_span)

pub fn type_to_string_simple(type_: Type) -> String {
  case type_ {
    Tvar(name, _) -> name
    Tfn(args, res, _) ->
      "(fn ["
      <> string.join(list.map(args, type_to_string_simple), with: " ")
      <> "] "
      <> type_to_string_simple(res)
      <> ")"
    Ttuple(args, _) ->
      "(, "
      <> string.join(list.map(args, type_to_string_simple), with: " ")
      <> ")"
    Tapp(target, args, _) ->
      "("
      <> type_to_string_simple(target)
      <> " "
      <> string.join(list.map(args, type_to_string_simple), with: " ")
      <> ")"
    Tcon(name, _) -> name
  }
}

pub fn type_to_string(type_: Type) -> String {
  case type_ {
    Tvar(name, _) -> name
    Tfn(args, result, _) -> {
      let args_s = list.map(args, type_to_string)
      "(fn ["
      <> string.join(args_s, with: " ")
      <> "] "
      <> type_to_string(result)
      <> ")"
    }
    Ttuple(args, _) ->
      "(, " <> string.join(list.map(args, type_to_string), with: " ") <> ")"
    Tapp(_, _, _) -> {
      let #(target, args) = unwrap_app(type_, [])
      let args_s = list.map(args, type_to_string)
      "("
      <> type_to_string(target)
      <> " "
      <> string.join(args_s, with: " ")
      <> ")"
    }
    Tcon(name, _) -> name
  }
}

pub fn unwrap_fn(type_: Type) -> #(List(Type), Type) {
  case type_ {
    Tfn(args, res, _) -> #(args, res)
    _ -> #([], type_)
  }
}

pub fn unwrap_app(type_: Type, args: List(Type)) -> #(Type, List(Type)) {
  case type_ {
    Tapp(a, b, _) -> unwrap_app(a, list.append(b, args))
    _ -> #(type_, args)
  }
}

pub fn type_free(type_: Type) -> set.Set(String) {
  case type_ {
    Tvar(id, _) -> set.insert(set.new(), id)
    Tcon(_, _) -> set.new()
    Tapp(a, b, _) ->
      list.fold(b, type_free(a), fn(acc, arg) { set.union(acc, type_free(arg)) })
    Ttuple(args, _) ->
      list.fold(args, set.new(), fn(acc, arg) { set.union(acc, type_free(arg)) })
    Tfn(args, res, _) ->
      list.fold(args, type_free(res), fn(acc, arg) {
        set.union(acc, type_free(arg))
      })
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
      Tapp(
        type_apply(subst, target),
        list.map(arg, fn(arg) { type_apply(subst, arg) }),
        loc,
      )
    Ttuple(args, loc) ->
      Ttuple(list.map(args, fn(arg) { type_apply(subst, arg) }), loc)
    Tfn(args, res, loc) ->
      Tfn(
        list.map(args, fn(arg) { type_apply(subst, arg) }),
        type_apply(subst, res),
        loc,
      )
    _ -> type_
  }
}

pub fn compose_subst(new_subst: Subst, old_subst: Subst) -> Subst {
  let applied_old =
    dict.map_values(old_subst, fn(_k, v) { type_apply(new_subst, v) })
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
    Tapp(a, b, loc) ->
      Tapp(
        type_con_to_var(vars, a),
        list.map(b, fn(arg) { type_con_to_var(vars, arg) }),
        loc,
      )
    Ttuple(args, loc) ->
      Ttuple(list.map(args, fn(arg) { type_con_to_var(vars, arg) }), loc)
    Tfn(args, res, loc) ->
      Tfn(
        list.map(args, fn(arg) { type_con_to_var(vars, arg) }),
        type_con_to_var(vars, res),
        loc,
      )
  }
}

pub fn type_unroll_app(type_: Type) -> #(Type, List(#(Type, Span))) {
  case type_ {
    Tapp(target, args, loc) -> #(
      target,
      list.map(args, fn(arg) { #(arg, loc) }),
    )
    Ttuple(_, _) -> #(type_, [])
    Tfn(_, _, _) -> #(type_, [])
    _ -> #(type_, [])
  }
}

pub fn tcon_and_args(
  type_: Type,
  coll: List(Type),
  loc: Int,
) -> #(String, List(Type)) {
  case type_ {
    Tvar(_, _) ->
      runtime.fatal(
        "Type not resolved "
        <> runtime.jsonify(loc)
        <> " "
        <> runtime.jsonify(type_)
        <> " "
        <> runtime.jsonify(coll),
      )
    Tcon(name, _) -> #(name, coll)
    Tapp(target, args, _) -> tcon_and_args(target, list.append(args, coll), loc)
    Ttuple(args, _) -> #("tuple", args)
    Tfn(_, _, _) ->
      runtime.fatal(
        "Function type not resolved "
        <> runtime.jsonify(loc)
        <> " "
        <> runtime.jsonify(type_),
      )
  }
}

pub fn type_resolve_aliases(
  aliases: dict.Dict(String, #(List(String), Type)),
  type_: Type,
) -> Type {
  case type_ {
    Ttuple(args, loc) ->
      Ttuple(
        list.map(args, fn(arg) { type_resolve_aliases(aliases, arg) }),
        loc,
      )
    Tfn(args, res, loc) ->
      Tfn(
        list.map(args, fn(arg) { type_resolve_aliases(aliases, arg) }),
        type_resolve_aliases(aliases, res),
        loc,
      )
    _ -> {
      let #(target, args) = type_unroll_app(type_)
      let resolved_args =
        list.map(args, fn(args) {
          let #(arg, _loc) = args
          type_resolve_aliases(aliases, arg)
        })

      case target {
        Tcon(name, loc) ->
          case dict.get(aliases, name) {
            Ok(#(free, alias_type)) -> {
              let subst = dict.from_list(list.zip(free, resolved_args))
              type_resolve_aliases(aliases, type_apply(subst, alias_type))
            }
            Error(_) ->
              case resolved_args {
                [] -> target
                _ -> Tapp(target, resolved_args, loc)
              }
          }
        Tvar(_, loc) ->
          case resolved_args {
            [] -> target
            _ -> Tapp(target, resolved_args, loc)
          }
        _ -> target
      }
    }
  }
}
