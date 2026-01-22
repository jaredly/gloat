import glance as g
import gleam/dict
import gleam/list
import gloat
import gloat/env
import gloat/type_error
import gloat/types

pub type HoverEntry =
  #(Int, Int, List(String))

pub type AnalysisError {
  ParseError(message: String)
  TypeError(message: String, start: Int, end: Int)
}

pub fn analyze(
  src: String,
  target: String,
  module_key: String,
  tenv: gloat.TEnv,
) -> Result(gloat.TEnv, AnalysisError) {
  case g.module(src) {
    Error(err) -> Error(ParseError(format_parse_error(err)))
    Ok(parsed) -> {
      case gloat.add_module_with_target(tenv, parsed, target, module_key) {
        Error(err) -> Error(to_type_error(err))
        Ok(env_) -> Ok(env_)
      }
    }
  }
}

pub fn hover_entries(env_: env.TEnv, module_key: String) -> List(HoverEntry) {
  case env.hover_for_module(env_, module_key) {
    Error(_) -> []
    Ok(hover_map) ->
      hover_map
      |> dict.to_list
      |> list.map(fn(entry) {
        let #(span, types_) = entry
        let g.Span(start, end) = span
        #(start, end, list.map(types_, types.type_to_string_gleam))
      })
  }
}

fn format_parse_error(error: g.Error) -> String {
  case error {
    g.UnexpectedEndOfInput -> "Unexpected end of input"
    g.UnexpectedToken(_token, _position) -> "Unexpected token"
  }
}

fn to_type_error(error: type_error.TypeError) -> AnalysisError {
  let g.Span(start, end) = type_error.span(error)
  TypeError(type_error.message(error), start, end)
}
