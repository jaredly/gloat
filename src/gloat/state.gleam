import gleam/dict
import gleam/list
import gloat/types

pub type StateData =
  #(Int, dict.Dict(String, types.Type), dict.Dict(types.Span, List(types.Type)))

pub type State(a) {
  State(run: fn(StateData) -> #(StateData, a))
}

pub fn run(state: State(a), data: StateData) -> #(StateData, a) {
  let State(run) = state
  run(data)
}

pub fn eval(state: State(a), data: StateData) -> a {
  let #(_data, value) = run(state, data)
  value
}

pub fn pure(value: a) -> State(a) {
  State(fn(state) { #(state, value) })
}

pub fn bind(state: State(a), f: fn(a) -> State(b)) -> State(b) {
  State(fn(data) {
    let #(next_data, value) = run(state, data)
    run(f(value), next_data)
  })
}

pub fn map(state: State(a), f: fn(a) -> b) -> State(b) {
  bind(state, fn(value) { pure(f(value)) })
}

pub fn get() -> State(StateData) {
  State(fn(state) { #(state, state) })
}

pub fn put(value: StateData) -> State(StateData) {
  State(fn(old) { #(value, old) })
}

pub fn map_list(list_: List(a), f: fn(a) -> State(b)) -> State(List(b)) {
  case list_ {
    [] -> pure([])
    [one, ..rest] ->
      bind(f(one), fn(mapped) {
        bind(map_list(rest, f), fn(rest_mapped) {
          pure([mapped, ..rest_mapped])
        })
      })
  }
}

pub fn foldl_list(list_: List(a), init: b, f: fn(b, a) -> State(b)) -> State(b) {
  case list_ {
    [] -> pure(init)
    [one, ..rest] -> bind(f(init, one), fn(next) { foldl_list(rest, next, f) })
  }
}

pub fn foldr_list(list_: List(a), init: b, f: fn(b, a) -> State(b)) -> State(b) {
  case list_ {
    [] -> pure(init)
    [one, ..rest] -> bind(foldr_list(rest, init, f), fn(next) { f(next, one) })
  }
}

pub fn each_list(list_: List(a), f: fn(a) -> State(Nil)) -> State(Nil) {
  case list_ {
    [] -> pure(Nil)
    [one, ..rest] -> bind(f(one), fn(_) { each_list(rest, f) })
  }
}

// pub const empty_state: StateData =

pub fn run_empty(state: State(a)) -> a {
  eval(state, #(0, dict.new(), dict.new()))
}

pub fn next_idx() -> State(Int) {
  bind(get(), fn(args) {
    let #(idx, subst, hover) = args
    bind(put(#(idx + 1, subst, hover)), fn(_) { pure(idx) })
  })
}

pub fn get_subst() -> State(dict.Dict(String, types.Type)) {
  bind(get(), fn(args) {
    let #(_, subst, _) = args
    pure(subst)
  })
}

pub fn put_subst(new_subst: dict.Dict(String, types.Type)) -> State(Nil) {
  bind(get(), fn(args) {
    let #(idx, subst, hover) = args
    let composed = types.compose_subst(new_subst, subst)
    bind(put(#(idx, composed, hover)), fn(_) { pure(Nil) })
  })
}

pub fn reset_subst(
  new_subst: dict.Dict(String, types.Type),
) -> State(dict.Dict(String, types.Type)) {
  bind(get(), fn(args) {
    let #(idx, old_subst, hover) = args
    bind(put(#(idx, new_subst, hover)), fn(_) { pure(old_subst) })
  })
}

pub fn get_hover() -> State(dict.Dict(types.Span, List(types.Type))) {
  bind(get(), fn(args) {
    let #(_, _, hover) = args
    pure(hover)
  })
}

pub fn put_hover(
  new_hover: dict.Dict(types.Span, List(types.Type)),
) -> State(Nil) {
  bind(get(), fn(args) {
    let #(idx, subst, _hover) = args
    bind(put(#(idx, subst, new_hover)), fn(_) { pure(Nil) })
  })
}

pub fn add_hover(span: types.Span, type_: types.Type) -> State(Nil) {
  bind(get_hover(), fn(hover) {
    let updated = case dict.get(hover, span) {
      Ok(existing) -> dict.insert(hover, span, list.append(existing, [type_]))
      Error(_) -> dict.insert(hover, span, [type_])
    }
    bind(put_hover(updated), fn(_) { pure(Nil) })
  })
}

pub fn reset_hover(
  new_hover: dict.Dict(types.Span, List(types.Type)),
) -> State(dict.Dict(types.Span, List(types.Type))) {
  bind(get(), fn(args) {
    let #(idx, subst, old_hover) = args
    bind(put(#(idx, subst, new_hover)), fn(_) { pure(old_hover) })
  })
}

pub fn reset_state() -> State(Nil) {
  bind(get(), fn(_) {
    bind(put(#(0, dict.new(), dict.new())), fn(_) { pure(Nil) })
  })
}

pub fn apply_with(
  f: fn(dict.Dict(String, types.Type), a) -> b,
  arg: a,
) -> State(b) {
  bind(get_subst(), fn(subst) { pure(f(subst, arg)) })
}
