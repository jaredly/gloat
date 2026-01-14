import glance as g
import gleam/list
import gleam/option
import gleam/result
import gloat/types

pub type Error {
  Unsupported(String)
}

pub fn type_(type_expr: g.Type) -> Result(types.Type, Error) {
  case type_expr {
    g.NamedType(span, name, module, parameters) ->
      case module {
        option.Some(_) -> Error(Unsupported("qualified type"))
        option.None -> {
          let resolved = case name {
            "BitArray" -> "BitString"
            _ -> name
          }
          result.map(result.all(list.map(parameters, type_)), fn(params) {
            case params {
              [] -> types.Tcon(resolved, span)
              _ -> types.Tapp(types.Tcon(resolved, span), params, span)
            }
          })
        }
      }

    g.VariableType(span, name) -> Ok(types.Tvar(name, span))

    g.FunctionType(span, parameters, return_type) ->
      map2(
        result.all(list.map(parameters, type_)),
        type_(return_type),
        fn(args, result_type) { types.Tfn(args, result_type, span) },
      )

    g.TupleType(span, elements) ->
      result.map(result.all(list.map(elements, type_)), fn(types_) {
        case types_ {
          [_, ..] -> Ok(types.Ttuple(types_, span))
          _ -> Error(Unsupported("tuple type arity"))
        }
      })
      |> result.flatten

    g.HoleType(_, _) -> Error(Unsupported("type hole"))
  }
}

pub fn variant_fields(
  fields: List(g.VariantField),
) -> Result(List(#(option.Option(String), types.Type)), Error) {
  list.map(fields, fn(field) {
    case field {
      g.LabelledVariantField(type_expr, label) ->
        result.map(type_(type_expr), fn(type_) { #(option.Some(label), type_) })
      g.UnlabelledVariantField(type_expr) ->
        result.map(type_(type_expr), fn(type_) { #(option.None, type_) })
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
