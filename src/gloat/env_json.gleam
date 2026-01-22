import glance as g
import gleam/dict
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/set
import gleam/string
import gloat/env
import gloat/scheme
import gloat/types

pub fn encode(tenv: env.TEnv) -> String {
  tenv
  |> to_json
  |> json.to_string
}

pub fn decode(input: String) -> Result(env.TEnv, String) {
  json.parse(input, tenv_decoder())
  |> result.map_error(fn(err) { string.inspect(err) })
}

fn to_json(tenv: env.TEnv) -> json.Json {
  let env.TEnv(
    values,
    tcons,
    types_map,
    aliases,
    modules,
    params,
    type_names,
    refinements,
    hover,
  ) = tenv
  json.object([
    #("values", dict_json(values, scheme_to_json)),
    #("tcons", dict_json(tcons, tcon_to_json)),
    #("types", dict_json(types_map, types_map_to_json)),
    #("aliases", dict_json(aliases, alias_to_json)),
    #("modules", dict_json(modules, json.string)),
    #("params", dict_json(params, params_to_json)),
    #("type_names", dict_json(type_names, json.string)),
    #("refinements", dict_json(refinements, json.string)),
    #("hover", dict_json(hover, hover_map_to_json)),
  ])
}

fn dict_json(dict_: dict.Dict(String, a), f: fn(a) -> json.Json) -> json.Json {
  json.dict(dict_, fn(key) { key }, f)
}

fn scheme_to_json(scheme_: scheme.Scheme) -> json.Json {
  let scheme.Forall(vars, type_) = scheme_
  json.object([
    #("vars", json.array(set.to_list(vars), json.string)),
    #("type", type_to_json(type_)),
  ])
}

fn type_to_json(type_: types.Type) -> json.Json {
  case type_ {
    types.Tvar(name, span) ->
      json.object([
        #("tag", json.string("Tvar")),
        #("name", json.string(name)),
        #("span", span_to_json(span)),
      ])
    types.Tcon(name, span) ->
      json.object([
        #("tag", json.string("Tcon")),
        #("name", json.string(name)),
        #("span", span_to_json(span)),
      ])
    types.Tapp(target, args, span) ->
      json.object([
        #("tag", json.string("Tapp")),
        #("target", type_to_json(target)),
        #("args", json.array(args, type_to_json)),
        #("span", span_to_json(span)),
      ])
    types.Ttuple(args, span) ->
      json.object([
        #("tag", json.string("Ttuple")),
        #("args", json.array(args, type_to_json)),
        #("span", span_to_json(span)),
      ])
    types.Tfn(args, result, span) ->
      json.object([
        #("tag", json.string("Tfn")),
        #("args", json.array(args, type_to_json)),
        #("result", type_to_json(result)),
        #("span", span_to_json(span)),
      ])
  }
}

fn span_to_json(span: g.Span) -> json.Json {
  let g.Span(start, end) = span
  json.array([start, end], json.int)
}

fn tcon_to_json(
  value: #(List(String), List(#(option.Option(String), types.Type)), types.Type),
) -> json.Json {
  let #(free, fields, result_type) = value
  json.object([
    #("free", json.array(free, json.string)),
    #("fields", json.array(fields, tcon_field_to_json)),
    #("result", type_to_json(result_type)),
  ])
}

fn tcon_field_to_json(field: #(option.Option(String), types.Type)) -> json.Json {
  let #(label, type_) = field
  json.object([
    #("label", json.nullable(label, json.string)),
    #("type", type_to_json(type_)),
  ])
}

fn types_map_to_json(value: #(Int, set.Set(String))) -> json.Json {
  let #(arity, constructors) = value
  json.object([
    #("arity", json.int(arity)),
    #("constructors", json.array(set.to_list(constructors), json.string)),
  ])
}

fn alias_to_json(value: #(List(String), types.Type)) -> json.Json {
  let #(free, type_) = value
  json.object([
    #("free", json.array(free, json.string)),
    #("type", type_to_json(type_)),
  ])
}

fn params_to_json(params: List(option.Option(String))) -> json.Json {
  json.array(params, fn(param) { json.nullable(param, json.string) })
}

fn hover_map_to_json(hover_map: env.HoverMap) -> json.Json {
  hover_map
  |> dict.to_list
  |> list.map(fn(entry) {
    let #(span, types_) = entry
    json.object([
      #("span", span_to_json(span)),
      #("types", json.array(types_, type_to_json)),
    ])
  })
  |> json.preprocessed_array
}

fn tenv_decoder() -> decode.Decoder(env.TEnv) {
  use values <- decode.field("values", dict_decoder(scheme_decoder()))
  use tcons <- decode.field("tcons", dict_decoder(tcon_decoder()))
  use types_map <- decode.field("types", dict_decoder(types_map_decoder()))
  use aliases <- decode.field("aliases", dict_decoder(alias_decoder()))
  use modules <- decode.field("modules", dict_decoder(decode.string))
  use params <- decode.field("params", dict_decoder(params_decoder()))
  use type_names <- decode.field("type_names", dict_decoder(decode.string))
  use refinements <- decode.field("refinements", dict_decoder(decode.string))
  use hover <- decode.field("hover", dict_decoder(hover_map_decoder()))
  decode.success(env.TEnv(
    values,
    tcons,
    types_map,
    aliases,
    modules,
    params,
    type_names,
    refinements,
    hover,
  ))
}

fn dict_decoder(
  value: decode.Decoder(a),
) -> decode.Decoder(dict.Dict(String, a)) {
  decode.dict(decode.string, value)
}

fn scheme_decoder() -> decode.Decoder(scheme.Scheme) {
  use vars <- decode.field("vars", decode.list(of: decode.string))
  use type_ <- decode.field("type", type_decoder())
  decode.success(scheme.Forall(set.from_list(vars), type_))
}

fn type_decoder() -> decode.Decoder(types.Type) {
  use tag <- decode.field("tag", decode.string)
  case tag {
    "Tvar" -> {
      use name <- decode.field("name", decode.string)
      use span <- decode.field("span", span_decoder())
      decode.success(types.Tvar(name, span))
    }
    "Tcon" -> {
      use name <- decode.field("name", decode.string)
      use span <- decode.field("span", span_decoder())
      decode.success(types.Tcon(name, span))
    }
    "Tapp" -> {
      use target <- decode.field("target", type_decoder())
      use args <- decode.field("args", decode.list(of: type_decoder()))
      use span <- decode.field("span", span_decoder())
      decode.success(types.Tapp(target, args, span))
    }
    "Ttuple" -> {
      use args <- decode.field("args", decode.list(of: type_decoder()))
      use span <- decode.field("span", span_decoder())
      decode.success(types.Ttuple(args, span))
    }
    "Tfn" -> {
      use args <- decode.field("args", decode.list(of: type_decoder()))
      use result_type <- decode.field("result", type_decoder())
      use span <- decode.field("span", span_decoder())
      decode.success(types.Tfn(args, result_type, span))
    }
    _ ->
      decode.failure(
        types.Tcon("Unknown", types.unknown_span),
        expected: "Type",
      )
  }
}

fn span_decoder() -> decode.Decoder(g.Span) {
  decode.list(of: decode.int)
  |> decode.then(fn(items) {
    case items {
      [start, end] -> decode.success(g.Span(start, end))
      _ -> decode.failure(g.Span(0, 0), expected: "Span")
    }
  })
}

fn tcon_decoder() -> decode.Decoder(
  #(List(String), List(#(option.Option(String), types.Type)), types.Type),
) {
  use free <- decode.field("free", decode.list(of: decode.string))
  use fields <- decode.field("fields", decode.list(of: tcon_field_decoder()))
  use result_type <- decode.field("result", type_decoder())
  decode.success(#(free, fields, result_type))
}

fn tcon_field_decoder() -> decode.Decoder(#(option.Option(String), types.Type)) {
  use label <- decode.field("label", decode.optional(decode.string))
  use type_ <- decode.field("type", type_decoder())
  decode.success(#(label, type_))
}

fn types_map_decoder() -> decode.Decoder(#(Int, set.Set(String))) {
  use arity <- decode.field("arity", decode.int)
  use constructors <- decode.field(
    "constructors",
    decode.list(of: decode.string),
  )
  decode.success(#(arity, set.from_list(constructors)))
}

fn alias_decoder() -> decode.Decoder(#(List(String), types.Type)) {
  use free <- decode.field("free", decode.list(of: decode.string))
  use type_ <- decode.field("type", type_decoder())
  decode.success(#(free, type_))
}

fn params_decoder() -> decode.Decoder(List(option.Option(String))) {
  decode.list(of: decode.optional(decode.string))
}

fn hover_map_decoder() -> decode.Decoder(env.HoverMap) {
  decode.list(of: hover_entry_decoder())
  |> decode.map(fn(entries) { dict.from_list(entries) })
}

fn hover_entry_decoder() -> decode.Decoder(#(g.Span, List(types.Type))) {
  use span <- decode.field("span", span_decoder())
  use types_ <- decode.field("types", decode.list(of: type_decoder()))
  decode.success(#(span, types_))
}
