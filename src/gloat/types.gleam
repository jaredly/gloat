import glance as g
import gleam/dict
import gleam/list
import gleam/set
import gleam/string
import gloat/type_error

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

pub fn type_to_string_gleam(type_: Type) -> String {
  type_to_string_gleam_inner(substitute_tvar_names(type_))
}

pub fn substitute_tvar_names(type_: Type) -> Type {
  let #(updated, _, _) = substitute_tvar_names_with_state(type_, dict.new(), 0)
  updated
}

fn substitute_tvar_names_with_state(
  type_: Type,
  names: dict.Dict(String, String),
  next_idx: Int,
) -> #(Type, dict.Dict(String, String), Int) {
  case type_ {
    Tvar(name, span) -> {
      case dict.get(names, name) {
        Ok(substituted) -> #(Tvar(substituted, span), names, next_idx)
        Error(_) -> {
          let substituted = tvar_name_from_index(next_idx)
          let names = dict.insert(names, name, substituted)
          #(Tvar(substituted, span), names, next_idx + 1)
        }
      }
    }
    Tcon(_, _) -> #(type_, names, next_idx)
    Ttuple(args, span) -> {
      let #(updated_args, names, next_idx) =
        substitute_tvar_names_list(args, names, next_idx)
      #(Ttuple(updated_args, span), names, next_idx)
    }
    Tfn(args, result, span) -> {
      let #(updated_args, names, next_idx) =
        substitute_tvar_names_list(args, names, next_idx)
      let #(updated_result, names, next_idx) =
        substitute_tvar_names_with_state(result, names, next_idx)
      #(Tfn(updated_args, updated_result, span), names, next_idx)
    }
    Tapp(target, args, span) -> {
      let #(updated_target, names, next_idx) =
        substitute_tvar_names_with_state(target, names, next_idx)
      let #(updated_args, names, next_idx) =
        substitute_tvar_names_list(args, names, next_idx)
      #(Tapp(updated_target, updated_args, span), names, next_idx)
    }
  }
}

fn substitute_tvar_names_list(
  args: List(Type),
  names: dict.Dict(String, String),
  next_idx: Int,
) -> #(List(Type), dict.Dict(String, String), Int) {
  list.fold(args, #([], names, next_idx), fn(acc, arg) {
    let #(rev_args, names, next_idx) = acc
    let #(updated_arg, names, next_idx) =
      substitute_tvar_names_with_state(arg, names, next_idx)
    #([updated_arg, ..rev_args], names, next_idx)
  })
  |> fn(result) {
    let #(rev_args, names, next_idx) = result
    #(list.reverse(rev_args), names, next_idx)
  }
}

fn tvar_name_from_index(idx: Int) -> String {
  let alphabet_length = 26
  let chars = tvar_name_loop(idx, alphabet_length, [])
  string.concat(chars)
}

fn tvar_name_loop(
  rest: Int,
  alphabet_length: Int,
  chars: List(String),
) -> List(String) {
  let n = rest % alphabet_length
  let rest = rest / alphabet_length
  let chars = [alphabet_letter(n), ..chars]
  case rest == 0 {
    True -> chars
    False -> tvar_name_loop(rest - 1, alphabet_length, chars)
  }
}

fn alphabet_letter(idx: Int) -> String {
  case idx {
    0 -> "a"
    1 -> "b"
    2 -> "c"
    3 -> "d"
    4 -> "e"
    5 -> "f"
    6 -> "g"
    7 -> "h"
    8 -> "i"
    9 -> "j"
    10 -> "k"
    11 -> "l"
    12 -> "m"
    13 -> "n"
    14 -> "o"
    15 -> "p"
    16 -> "q"
    17 -> "r"
    18 -> "s"
    19 -> "t"
    20 -> "u"
    21 -> "v"
    22 -> "w"
    23 -> "x"
    24 -> "y"
    25 -> "z"
    _ -> "a"
  }
}

fn type_to_string_gleam_inner(type_: Type) -> String {
  case type_ {
    Tvar(name, _) -> name
    Tcon(name, _) ->
      case strip_module_prefix(name) {
        "BitString" -> "BitArray"
        stripped -> stripped
      }
    Ttuple(args, _) ->
      "#("
      <> string.join(list.map(args, type_to_string_gleam_inner), with: ", ")
      <> ")"
    Tfn(args, result, _) ->
      "fn("
      <> string.join(list.map(args, type_to_string_gleam_inner), with: ", ")
      <> ") -> "
      <> type_to_string_gleam_inner(result)
    Tapp(_, _, _) -> {
      let #(target, args) = unwrap_app(type_, [])
      let target_s = type_to_string_gleam_target(target)
      case args {
        [] -> target_s
        _ ->
          target_s
          <> "("
          <> string.join(list.map(args, type_to_string_gleam_inner), with: ", ")
          <> ")"
      }
    }
  }
}

fn strip_module_prefix(name: String) -> String {
  let parts = string.split(name, "/")
  case list.reverse(parts) {
    [last, ..] -> last
    [] -> name
  }
}

fn type_to_string_gleam_target(type_: Type) -> String {
  case type_ {
    Tcon(name, _) -> strip_module_prefix(name)
    Tvar(name, _) -> name
    _ -> "(" <> type_to_string_gleam_inner(type_) <> ")"
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
  span: g.Span,
) -> Result(#(String, List(Type)), type_error.TypeError) {
  case type_ {
    Tvar(_, _) -> Error(type_error.new("Type not resolved", span))
    Tcon(name, _) -> Ok(#(name, coll))
    Tapp(target, args, _) ->
      tcon_and_args(target, list.append(args, coll), span)
    Ttuple(args, _) -> Ok(#("tuple", args))
    Tfn(_, _, _) -> Error(type_error.new("Function type not resolved", span))
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
