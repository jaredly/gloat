import glance as g
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option
import gleam/set
import gloat/env
import gloat/infer_state as is
import gloat/literals
import gloat/types

pub type ExPattern {
  ExAny
  ExConstructor(String, String, List(ExPattern))
  ExOr(ExPattern, ExPattern)
}

pub fn check_exhaustiveness(
  tenv: env.TEnv,
  target_type: types.Type,
  patterns: List(g.Pattern),
  span: g.Span,
) -> is.InferState(Nil) {
  use applied_target <- is.bind(type_apply_state(target_type))
  use matrix <- is.bind(
    is.map_list(patterns, fn(pat) {
      use ex <- is.bind(pattern_to_ex_pattern(tenv, #(pat, applied_target)))
      is.ok([ex])
    }),
  )

  case is_exhaustive(tenv, matrix) {
    True -> is.ok(Nil)
    False -> is.error("Match not exhaustive", span)
  }
}

pub fn pattern_to_ex_pattern(
  tenv: env.TEnv,
  pat_and_type: #(g.Pattern, types.Type),
) -> is.InferState(ExPattern) {
  let #(pattern, type_) = pat_and_type
  case pattern {
    g.PatternVariable(_, _) -> is.ok(ExAny)
    g.PatternDiscard(_, _) -> is.ok(ExAny)
    g.PatternString(_, str) -> is.ok(ExConstructor(str, "String", []))
    g.PatternInt(span, value) ->
      case literals.parse_int_literal(value) {
        Ok(v) -> is.ok(ExConstructor(int.to_string(v), "Int", []))
        Error(_) -> is.error("Invalid int pattern", span)
      }
    g.PatternFloat(span, value) ->
      case float.parse(value) {
        Ok(v) -> is.ok(ExConstructor(float.to_string(v), "Float", []))
        Error(_) -> is.error("Invalid float pattern", span)
      }
    g.PatternAssignment(_span, pat, _name) ->
      pattern_to_ex_pattern(tenv, #(pat, type_))
    g.PatternConcatenate(_, _prefix, _prefix_name, _rest_name) -> is.ok(ExAny)
    g.PatternList(span, items, tail) ->
      case type_ {
        types.Tapp(types.Tcon("List", _), [elem_type], _) -> {
          use base <- is.bind(case tail {
            option.None -> is.ok(ExConstructor("[]", "List", []))
            option.Some(pat) -> pattern_to_ex_pattern(tenv, #(pat, type_))
          })
          is.foldr_list(items, base, fn(acc, item) {
            use head <- is.bind(pattern_to_ex_pattern(tenv, #(item, elem_type)))
            is.ok(ExConstructor("::", "List", [head, acc]))
          })
        }
        _ -> is.error("List pattern with non-list type", span)
      }
    g.PatternBitString(_, _segments) ->
      is.ok(ExConstructor("bitstring", "BitString", []))
    g.PatternTuple(span, items) ->
      case type_ {
        types.Ttuple(targs, _) ->
          case list.length(items) == list.length(targs) {
            True ->
              is.map(
                is.map_list(list.zip(items, targs), fn(pair) {
                  let #(pat, t) = pair
                  pattern_to_ex_pattern(tenv, #(pat, t))
                }),
                fn(pats) { ExConstructor(",", "tuple", pats) },
              )
            False -> is.error("Tuple arity mismatch", span)
          }
        _ -> is.error("Tuple pattern with non-tuple type", span)
      }
    g.PatternVariant(span, module, name, arguments, with_spread) -> {
      use tcon <- is.bind(is.from_result(types.tcon_and_args(type_, [], span)))
      let #(tname, targs) = tcon
      let env.TEnv(
        _values,
        tcons,
        _types,
        _aliases,
        _modules,
        _params,
        _type_names,
      ) = tenv
      let constructor_name = case module {
        option.None -> Ok(name)
        option.Some(module_name) ->
          case env.resolve_module(tenv, module_name) {
            Ok(module_key) -> Ok(module_key <> "/" <> name)
            Error(_) -> Error(Nil)
          }
      }
      case constructor_name {
        Error(_) -> is.error("Unknown type constructor " <> name, span)
        Ok(constructor_name) ->
          case dict.get(tcons, constructor_name) {
            Error(_) ->
              is.error("Unknown type constructor " <> constructor_name, span)
            Ok(value) -> {
              let #(free_names, cargs, _cres) = value
              let subst = dict.from_list(list.zip(free_names, targs))
              let fields =
                list.map(arguments, fn(field) { pattern_field(field) })
              use aligned <- is.bind(align_constructor_fields(
                fields,
                cargs,
                span,
                with_spread,
              ))
              use converted_args <- is.bind(
                is.map_list(list.zip(aligned, cargs), fn(pair) {
                  let #(maybe_pat, cfield) = pair
                  let #(_label, ctype) = cfield
                  case maybe_pat {
                    option.None -> is.ok(ExAny)
                    option.Some(pat) ->
                      pattern_to_ex_pattern(tenv, #(
                        pat,
                        types.type_apply(subst, ctype),
                      ))
                  }
                }),
              )

              is.ok(ExConstructor(constructor_name, tname, converted_args))
            }
          }
      }
    }
  }
}

fn pattern_field(
  field: g.Field(g.Pattern),
) -> #(option.Option(String), g.Pattern) {
  case field {
    g.UnlabelledField(item) -> #(option.None, item)
    g.LabelledField(label, _loc, item) -> #(option.Some(label), item)
    g.ShorthandField(label, loc) -> #(
      option.Some(label),
      g.PatternVariable(loc, label),
    )
  }
}

fn align_constructor_fields(
  fields: List(#(option.Option(String), g.Pattern)),
  cfields: List(#(option.Option(String), types.Type)),
  span: g.Span,
  allow_missing: Bool,
) -> is.InferState(List(option.Option(g.Pattern))) {
  let indexed = index_list(cfields)
  let labels =
    list.filter_map(indexed, fn(entry) {
      let #(idx, #(label, _type)) = entry
      case label {
        option.Some(name) -> Ok(#(name, idx))
        option.None -> Error(Nil)
      }
    })
  let labels_dict = dict.from_list(labels)
  let unused =
    list.map(indexed, fn(entry) {
      let #(idx, _field) = entry
      idx
    })

  use folded <- is.bind(
    is.foldl_list(fields, #(dict.new(), unused), fn(acc, field) {
      let #(assigned, unused_inner) = acc
      let #(label, pat) = field
      case label {
        option.Some(name) ->
          case dict.get(labels_dict, name) {
            Ok(idx) ->
              case list.contains(unused_inner, idx) {
                True ->
                  is.ok(#(
                    dict.insert(assigned, idx, pat),
                    list.filter(unused_inner, fn(i) { i != idx }),
                  ))
                False -> is.error("Duplicate field " <> name, span)
              }
            Error(_) -> is.error("Unknown field " <> name, span)
          }
        option.None ->
          case unused_inner {
            [] -> is.error("Constructor field mismatch", span)
            [first, ..rest] -> is.ok(#(dict.insert(assigned, first, pat), rest))
          }
      }
    }),
  )
  let #(assigned, remaining) = folded

  use _ignored_remaining <- is.bind(case remaining {
    [] -> is.ok(Nil)
    _ ->
      case allow_missing {
        True -> is.ok(Nil)
        False -> is.error("Constructor field mismatch", span)
      }
  })

  is.ok(
    list.map(indexed, fn(entry) {
      let #(idx, _field) = entry
      case dict.get(assigned, idx) {
        Ok(pat) -> option.Some(pat)
        Error(_) -> option.None
      }
    }),
  )
}

fn index_list(items: List(a)) -> List(#(Int, a)) {
  let #(_idx, acc) =
    list.fold(items, #(0, []), fn(acc, item) {
      let #(idx, items) = acc
      #(idx + 1, [#(idx, item), ..items])
    })
  list.reverse(acc)
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
    [] -> []
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
              True -> Error(Nil)
              False -> Ok(id)
            }
        }
      _ -> gid
    }
  })
}

fn group_constructors(tenv: env.TEnv, gid: String) -> List(String) {
  case gid {
    "Int" -> []
    "Float" -> []
    "Bool" -> ["True", "False"]
    "String" -> []
    "List" -> ["[]", "::"]
    "BitString" -> []
    "tuple" -> [","]
    _ -> {
      let env.TEnv(
        _values,
        _tcons,
        types,
        _aliases,
        _modules,
        _params,
        _type_names,
      ) = tenv
      case dict.get(types, gid) {
        Error(_) -> []
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
        [] -> ExAny
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

fn type_apply_state(type_: types.Type) -> is.InferState(types.Type) {
  is.apply_with(types.type_apply, type_)
}
