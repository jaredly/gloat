import glance as g
import gleam/dict
import gloat/state
import gloat/type_error
import gloat/types

pub type InferState(a) =
  state.State(Result(a, type_error.TypeError))

pub fn ok(value: a) -> InferState(a) {
  state.pure(Ok(value))
}

pub fn error(message: String, span: g.Span) -> InferState(a) {
  state.pure(Error(type_error.new(message, span)))
}

pub fn bind(st: InferState(a), f: fn(a) -> InferState(b)) -> InferState(b) {
  state.bind(st, fn(result) {
    case result {
      Ok(value) -> f(value)
      Error(err) -> state.pure(Error(err))
    }
  })
}

pub fn map(st: InferState(a), f: fn(a) -> b) -> InferState(b) {
  bind(st, fn(value) { ok(f(value)) })
}

pub fn map_list(
  list_: List(a),
  f: fn(a) -> InferState(b),
) -> InferState(List(b)) {
  case list_ {
    [] -> ok([])
    [one, ..rest] ->
      bind(f(one), fn(mapped) {
        bind(map_list(rest, f), fn(rest_mapped) { ok([mapped, ..rest_mapped]) })
      })
  }
}

pub fn each_list(list_: List(a), f: fn(a) -> InferState(Nil)) -> InferState(Nil) {
  case list_ {
    [] -> ok(Nil)
    [one, ..rest] -> bind(f(one), fn(_) { each_list(rest, f) })
  }
}

pub fn foldl_list(
  list_: List(a),
  init: b,
  f: fn(b, a) -> InferState(b),
) -> InferState(b) {
  case list_ {
    [] -> ok(init)
    [one, ..rest] -> bind(f(init, one), fn(next) { foldl_list(rest, next, f) })
  }
}

pub fn foldr_list(
  list_: List(a),
  init: b,
  f: fn(b, a) -> InferState(b),
) -> InferState(b) {
  case list_ {
    [] -> ok(init)
    [one, ..rest] -> bind(foldr_list(rest, init, f), fn(next) { f(next, one) })
  }
}

pub fn lift(st: state.State(a)) -> InferState(a) {
  state.map(st, fn(value) { Ok(value) })
}

pub fn from_result(res: Result(a, type_error.TypeError)) -> InferState(a) {
  state.pure(res)
}

pub fn run_empty(st: InferState(a)) -> Result(a, type_error.TypeError) {
  state.run_empty(st)
}

pub fn run_empty_with_hover(
  st: InferState(a),
) -> #(Result(a, type_error.TypeError), dict.Dict(types.Span, List(types.Type))) {
  let #(data, result) = state.run(st, #(0, dict.new(), dict.new()))
  let #(_idx, _subst, hover) = data
  #(result, hover)
}

pub fn add_hover(span: types.Span, type_: types.Type) -> InferState(Nil) {
  lift(state.add_hover(span, type_))
}

pub fn apply_with(
  f: fn(dict.Dict(String, types.Type), a) -> b,
  arg: a,
) -> InferState(b) {
  lift(state.apply_with(f, arg))
}
