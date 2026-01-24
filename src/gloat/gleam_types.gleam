import glance as g
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gloat/env
import gloat/types

pub type Error {
  Unsupported(String)
}

pub fn type_(tenv: env.TEnv, type_expr: g.Type) -> Result(types.Type, Error) {
  let env.TEnv(
    _values,
    _tcons,
    _types,
    aliases,
    _modules,
    _params,
    _type_names,
    _refinements,
    _hover,
  ) = tenv
  case type_expr {
    g.NamedType(span, name, module, parameters) -> {
      let resolved = case name {
        "BitArray" -> "BitString"
        _ -> name
      }
      let qualified = case module {
        option.None ->
          case env.resolve_type_name(tenv, resolved) {
            Ok(name) -> name
            Error(_) ->
              case env.type_exists(tenv, resolved) {
                True -> resolved
                False -> resolved
              }
          }
        option.Some(module_name) ->
          case env.resolve_module(tenv, module_name) {
            Ok(module_key) -> module_key <> "/" <> resolved
            Error(_) -> module_name <> "/" <> resolved
          }
      }
      result.map(
        result.all(list.map(parameters, fn(param) { type_(tenv, param) })),
        fn(params) {
          case params {
            [] -> types.Tcon(qualified, span)
            _ -> types.Tapp(types.Tcon(qualified, span), params, span)
          }
        },
      )
      |> result.map(fn(t) { types.type_resolve_aliases(aliases, t) })
    }

    g.VariableType(span, name) -> Ok(types.Tvar(name, span))

    g.FunctionType(span, parameters, return_type) ->
      map2(
        result.all(list.map(parameters, fn(param) { type_(tenv, param) })),
        type_(tenv, return_type),
        fn(args, result_type) { types.Tfn(args, result_type, span) },
      )

    g.TupleType(span, elements) ->
      result.map(
        result.all(list.map(elements, fn(elem) { type_(tenv, elem) })),
        fn(types_) {
          case types_ {
            [_, ..] -> Ok(types.Ttuple(types_, span))
            _ -> Error(Unsupported("tuple type arity"))
          }
        },
      )
      |> result.flatten

    g.HoleType(span, _label) ->
      Ok(types.Tvar("hole_" <> int.to_string(loc_from_span(span)), span))
  }
}

pub fn variant_fields(
  tenv: env.TEnv,
  fields: List(g.VariantField),
) -> Result(List(#(option.Option(String), types.Type)), Error) {
  list.map(fields, fn(field) {
    case field {
      g.LabelledVariantField(type_expr, label) ->
        result.map(type_(tenv, type_expr), fn(type_) {
          #(option.Some(label), type_)
        })
      g.UnlabelledVariantField(type_expr) ->
        result.map(type_(tenv, type_expr), fn(type_) { #(option.None, type_) })
    }
  })
  |> result.all
}

pub fn loc_from_span(span: g.Span) -> Int {
  let g.Span(start, _end) = span
  start
}

fn map2(a: Result(x, e), b: Result(y, e), f: fn(x, y) -> z) -> Result(z, e) {
  result.try(a, fn(av) { result.map(b, fn(bv) { f(av, bv) }) })
}
