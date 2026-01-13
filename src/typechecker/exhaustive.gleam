import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/set
import typechecker/ast
import typechecker/env
import typechecker/runtime
import typechecker/state
import typechecker/types

pub type ExPattern {
  ExAny
  ExConstructor(String, String, List(ExPattern))
  ExOr(ExPattern, ExPattern)
}

pub fn check_exhaustiveness(
  tenv: env.TEnv,
  target_type: types.Type,
  patterns: List(ast.Pat),
  loc: Int,
) -> state.State(Nil) {
  use applied_target <- state.bind(type_apply_state(target_type))
  let matrix =
    list.map(patterns, fn(pat) {
      [pattern_to_ex_pattern(tenv, #(pat, applied_target))]
    })

  case is_exhaustive(tenv, matrix) {
    True -> state.pure(Nil)
    False -> runtime.fatal("Match not exhaustive " <> int.to_string(loc))
  }
}

pub fn pattern_to_ex_pattern(
  tenv: env.TEnv,
  pat_and_type: #(ast.Pat, types.Type),
) -> ExPattern {
  let #(pattern, type_) = pat_and_type
  case pattern {
    ast.Pvar(_, _) -> ExAny
    ast.Pany(_) -> ExAny
    ast.Pstr(str, _) -> ExConstructor(str, "string", [])
    ast.Pprim(ast.Pint(v, _), _) -> ExConstructor(int.to_string(v), "int", [])
    ast.Pprim(ast.Pfloat(v, _), _) ->
      ExConstructor(float.to_string(v), "float", [])
    ast.Pprim(ast.Pbool(v, _), _) ->
      ExConstructor(
        case v {
          True -> "true"
          False -> "false"
        },
        "bool",
        [],
      )
    ast.Pas(_name, pat, _loc) -> pattern_to_ex_pattern(tenv, #(pat, type_))
    ast.Plist(items, tail, loc) ->
      case type_ {
        types.Tapp(types.Tcon("list", _), elem_type, _) -> {
          let base = case tail {
            option.None -> ExConstructor("[]", "list", [])
            option.Some(pat) -> pattern_to_ex_pattern(tenv, #(pat, type_))
          }
          list.fold_right(items, base, fn(acc, item) {
            let head = pattern_to_ex_pattern(tenv, #(item, elem_type))
            ExConstructor("::", "list", [head, acc])
          })
        }
        _ ->
          runtime.fatal(
            "List pattern with non-list type " <> int.to_string(loc),
          )
      }
    ast.Ptuple(items, loc) ->
      case type_ {
        types.Ttuple(targs, _) ->
          case list.length(items) == list.length(targs) {
            True ->
              ExConstructor(
                ",",
                "tuple",
                list.map(list.zip(items, targs), fn(pair) {
                  let #(pat, t) = pair
                  pattern_to_ex_pattern(tenv, #(pat, t))
                }),
              )
            False ->
              runtime.fatal("Tuple arity mismatch " <> int.to_string(loc))
          }
        _ -> runtime.fatal("Tuple pattern with non-tuple type")
      }
    ast.Pcon(name, _name_loc, args, loc) -> {
      let #(tname, targs) = types.tcon_and_args(type_, [], loc)
      let env.TEnv(_values, tcons, _types, _aliases) = tenv

      let #(free_names, cargs, _cres) = case dict.get(tcons, name) {
        Error(_) -> runtime.fatal("Unknown type constructor " <> name)
        Ok(value) -> value
      }

      let subst = dict.from_list(list.zip(free_names, targs))
      let converted_args =
        list.map(
          list.zip(args, list.map(cargs, fn(t) { types.type_apply(subst, t) })),
          fn(pair) {
            let #(pat, type_) = pair
            pattern_to_ex_pattern(tenv, #(pat, type_))
          },
        )

      ExConstructor(name, tname, converted_args)
    }
  }
}

fn any_list(arity: Int) -> List(ExPattern) {
  case arity == 0 {
    True -> []
    False -> [ExAny, ..any_list(arity - 1)]
  }
}

fn default_matrix(matrix: List(List(ExPattern))) -> List(List(ExPattern)) {
  list.flatten(
    list.map(matrix, fn(row) {
      case row {
        [ExAny, ..rest] -> [rest]
        [ExOr(left, right), ..rest] ->
          default_matrix([[left, ..rest], [right, ..rest]])
        _ -> []
      }
    }),
  )
}

fn is_exhaustive(tenv: env.TEnv, matrix: List(List(ExPattern))) -> Bool {
  !is_useful(tenv, matrix, [ExAny])
}

fn specialized_matrix(
  constructor: String,
  arity: Int,
  matrix: List(List(ExPattern)),
) -> List(List(ExPattern)) {
  list.flatten(
    list.map(matrix, fn(row) { specialize_row(constructor, arity, row) }),
  )
}

fn specialize_row(
  constructor: String,
  arity: Int,
  row: List(ExPattern),
) -> List(List(ExPattern)) {
  case row {
    [] -> runtime.fatal("Can't specialize an empty row.")
    [ExAny, ..rest] -> [list.append(any_list(arity), rest)]
    [ExConstructor(name, _, args), ..rest] ->
      case name == constructor {
        True -> [list.append(args, rest)]
        False -> []
      }
    [ExOr(left, right), ..rest] ->
      specialized_matrix(constructor, arity, [[left, ..rest], [right, ..rest]])
  }
}

fn fold_ex_pat(init: a, pat: ExPattern, f: fn(a, ExPattern) -> a) -> a {
  case pat {
    ExOr(left, right) -> f(f(init, left), right)
    _ -> f(init, pat)
  }
}

fn fold_ex_pats(init: a, pats: List(ExPattern), f: fn(a, ExPattern) -> a) -> a {
  list.fold(pats, init, fn(acc, pat) { fold_ex_pat(acc, pat, f) })
}

fn find_gid(heads: List(ExPattern)) -> Result(String, Nil) {
  fold_ex_pats(Error(Nil), heads, fn(gid, pat) {
    case pat {
      ExConstructor(_, id, _) ->
        case gid {
          Error(_) -> Ok(id)
          Ok(existing) ->
            case existing != id {
              True ->
                runtime.fatal(
                  "Constructors with different group IDs in the same position.",
                )
              False -> Ok(id)
            }
        }
      _ -> gid
    }
  })
}

fn group_constructors(tenv: env.TEnv, gid: String) -> List(String) {
  case gid {
    "int" -> []
    "float" -> []
    "bool" -> ["true", "false"]
    "string" -> []
    "list" -> ["[]", "::"]
    "tuple" -> [","]
    _ -> {
      let env.TEnv(_values, _tcons, types, _aliases) = tenv
      case dict.get(types, gid) {
        Error(_) -> runtime.fatal("Unknown type name " <> gid)
        Ok(#(_arity, names)) -> set.to_list(names)
      }
    }
  }
}

fn args_if_complete(
  tenv: env.TEnv,
  matrix: List(List(ExPattern)),
) -> dict.Dict(String, Int) {
  let heads =
    list.map(matrix, fn(row) {
      case row {
        [] -> runtime.fatal("is_complete called with empty row")
        [head, ..] -> head
      }
    })

  case find_gid(heads) {
    Error(_) -> dict.new()
    Ok(gid) -> {
      let found =
        list.fold(heads, [], fn(acc, head) {
          case head {
            ExConstructor(id, _, args) -> [#(id, list.length(args)), ..acc]
            _ -> acc
          }
        })
      let found_dict = dict.from_list(found)

      case group_constructors(tenv, gid) {
        [] -> dict.new()
        constrs ->
          list.fold(constrs, found_dict, fn(acc, id) {
            case dict.get(acc, id) {
              Error(_) -> dict.new()
              Ok(_) -> acc
            }
          })
      }
    }
  }
}

fn is_useful(
  tenv: env.TEnv,
  matrix: List(List(ExPattern)),
  row: List(ExPattern),
) -> Bool {
  let head_and_rest = case matrix {
    [] -> Error(Nil)
    [[], ..] -> Error(Nil)
    [_, ..] ->
      case row {
        [] -> Error(Nil)
        [head, ..rest] -> Ok(#(head, rest))
      }
  }

  case head_and_rest {
    Error(_) -> False
    Ok(#(head, rest)) ->
      case head {
        ExConstructor(id, _, args) ->
          is_useful(
            tenv,
            specialized_matrix(id, list.length(args), matrix),
            list.append(args, rest),
          )
        ExAny ->
          case dict.to_list(args_if_complete(tenv, matrix)) {
            [] ->
              case default_matrix(matrix) {
                [] -> True
                defaults -> is_useful(tenv, defaults, rest)
              }
            alts ->
              list.any(alts, fn(tuple) {
                let #(id, alt) = tuple
                is_useful(
                  tenv,
                  specialized_matrix(id, alt, matrix),
                  list.append(any_list(alt), rest),
                )
              })
          }
        ExOr(left, right) ->
          case is_useful(tenv, matrix, [left, ..rest]) {
            True -> True
            False -> is_useful(tenv, matrix, [right, ..rest])
          }
      }
  }
}

fn type_apply_state(type_: types.Type) -> state.State(types.Type) {
  state.apply_with(types.type_apply, type_)
}
