import glance as g
import gleam/dict
import gleam/list
import gleam/option
import gleam/result
import gleam/set
import gleam/string
import gloat
import gloat/env
import gloat/glance as gloat_glance
import gloat/scheme
import gloat/type_error
import gloat/types

type TypeOrParseError {
  Type(gloat.TypeError)
  Parse(g.Error)
}

fn wrap_parse(res: Result(a, g.Error)) {
  case res {
    Ok(v) -> Ok(v)
    Error(err) -> Error(Parse(err))
  }
}

fn wrap_type(res: Result(a, gloat.TypeError)) {
  case res {
    Ok(v) -> Ok(v)
    Error(err) -> Error(Type(err))
  }
}

fn assert_module_infer(
  code: String,
) -> Result(List(#(String, String)), TypeOrParseError) {
  infer_module(code, [])
}

fn assert_module_infer_with_deps(
  deps: List(#(String, String)),
  code: String,
) -> Result(List(#(String, String)), TypeOrParseError) {
  infer_module(code, deps)
}

fn infer_module(
  code: String,
  deps: List(#(String, String)),
) -> Result(List(#(String, String)), TypeOrParseError) {
  let base_env = gloat.builtin_env()
  result.try(wrap_type(add_deps(base_env, deps)), fn(env_with_deps) {
    use parsed <- result.try(wrap_parse(g.module(code)))
    let filtered = gloat_glance.filter_module_for_target(parsed, "erlang")
    wrap_type(
      result.try(
        gloat.add_module_with_target(env_with_deps, filtered, "erlang"),
        fn(env2) {
          let names = public_export_names(filtered)
          result.map(
            result.all(
              list.map(names, fn(name) {
                case env.resolve(env2, name) {
                  Ok(scheme_) -> Ok(#(name, gloat.scheme_to_string(scheme_)))
                  Error(_) ->
                    Error(type_error.new(
                      "definition not found in env: " <> name,
                      types.unknown_span,
                    ))
                }
              }),
            ),
            sort_pairs,
          )
        },
      ),
    )
  })
}

fn add_deps(
  base: env.TEnv,
  deps: List(#(String, String)),
) -> Result(env.TEnv, gloat.TypeError) {
  list.fold(deps, Ok(base), fn(acc, dep) {
    result.try(acc, fn(env_acc) {
      let #(module_name, src) = dep
      let assert Ok(parsed) = g.module(src)
      let filtered = gloat_glance.filter_module_for_target(parsed, "erlang")
      result.try(
        gloat.add_module_with_target(env_acc, filtered, "erlang"),
        fn(dep_env) {
          let qualified = qualify_dep_env(dep_env, filtered, module_name)
          Ok(env.merge(env_acc, qualified))
        },
      )
    })
  })
}

fn qualify_dep_env(
  dep_env: env.TEnv,
  module: g.Module,
  module_name: String,
) -> env.TEnv {
  let prefix = module_key(module_name) <> "/"
  let module_type_names = module_type_name_set(module)
  let public_values = public_value_names(module)
  let public_constructors = public_constructor_names(module)
  let public_aliases = public_type_alias_names(module)
  let public_custom_types = public_custom_type_names(module)

  let env.TEnv(
    _values,
    tcons,
    types_map,
    aliases,
    _modules,
    _params,
    _names,
    _refinements,
  ) = dep_env

  let values_dict =
    list.fold(public_values, dict.new(), fn(acc, name) {
      case env.resolve(dep_env, name) {
        Ok(scheme_) -> {
          let qualified = prefix <> name
          let updated = qualify_scheme(scheme_, module_type_names, prefix)
          dict.insert(acc, qualified, updated)
        }
        Error(_) -> acc
      }
    })

  let tcons_dict =
    list.fold(public_constructors, dict.new(), fn(acc, name) {
      case dict.get(tcons, name) {
        Ok(constructor) -> {
          let qualified = prefix <> name
          let updated = qualify_tcon(constructor, module_type_names, prefix)
          dict.insert(acc, qualified, updated)
        }
        Error(_) -> acc
      }
    })

  let params_dict =
    list.fold(
      list.append(public_values, public_constructors),
      dict.new(),
      fn(acc, name) {
        let qualified = prefix <> name
        case env.resolve_params(dep_env, name) {
          Ok(params) -> dict.insert(acc, qualified, params)
          Error(_) -> acc
        }
      },
    )

  let types_dict =
    list.fold(public_custom_types, dict.new(), fn(acc, name) {
      case dict.get(types_map, name) {
        Ok(#(arity, names)) -> {
          let qualified_names =
            names
            |> set.to_list
            |> list.map(fn(n) { prefix <> n })
            |> set.from_list
          dict.insert(acc, prefix <> name, #(arity, qualified_names))
        }
        Error(_) -> acc
      }
    })

  let alias_dict =
    list.fold(public_aliases, dict.new(), fn(acc, name) {
      case dict.get(aliases, name) {
        Ok(#(free, alias_type)) -> {
          let qualified = prefix <> name
          let updated = qualify_type(alias_type, module_type_names, prefix)
          dict.insert(acc, qualified, #(free, updated))
        }
        Error(_) -> acc
      }
    })

  env.TEnv(
    values_dict,
    tcons_dict,
    types_dict,
    alias_dict,
    dict.new(),
    params_dict,
    dict.new(),
    dict.new(),
  )
}

fn module_key(module_name: String) -> String {
  let parts = string.split(module_name, "/")
  case list.reverse(parts) {
    [last, ..] -> last
    [] -> module_name
  }
}

fn filter_map_option(items: List(a), f: fn(a) -> option.Option(b)) -> List(b) {
  list.fold(items, [], fn(acc, item) {
    case f(item) {
      option.Some(value) -> [value, ..acc]
      option.None -> acc
    }
  })
  |> list.reverse
}

fn public_export_names(module: g.Module) -> List(String) {
  let values = public_value_names(module)
  let constructors = public_constructor_names(module)
  list.append(values, constructors)
}

fn public_value_names(module: g.Module) -> List(String) {
  let g.Module(_imports, _types, _aliases, constants, functions) = module
  let fn_names =
    filter_map_option(functions, fn(defn) {
      let g.Definition(_attrs, function) = defn
      let g.Function(_span, name, publicity, _params, _return, _body) = function
      case publicity {
        g.Public -> option.Some(name)
        g.Private -> option.None
      }
    })
  let const_names =
    filter_map_option(constants, fn(defn) {
      let g.Definition(_attrs, constant) = defn
      let g.Constant(_span, name, publicity, _annotation, _value) = constant
      case publicity {
        g.Public -> option.Some(name)
        g.Private -> option.None
      }
    })
  list.append(fn_names, const_names)
}

fn public_constructor_names(module: g.Module) -> List(String) {
  let g.Module(_imports, custom_types, _aliases, _constants, _functions) =
    module
  list.fold(custom_types, [], fn(acc, defn) {
    let g.Definition(_attrs, custom_type) = defn
    let g.CustomType(_span, _name, publicity, opaque_, _params, variants) =
      custom_type
    case publicity, opaque_ {
      g.Public, False ->
        list.append(
          acc,
          list.map(variants, fn(variant) {
            let g.Variant(name, _fields, _attrs) = variant
            name
          }),
        )
      _, _ -> acc
    }
  })
}

fn public_type_alias_names(module: g.Module) -> List(String) {
  let g.Module(_imports, _types, type_aliases, _constants, _functions) = module
  filter_map_option(type_aliases, fn(defn) {
    let g.Definition(_attrs, alias) = defn
    let g.TypeAlias(_span, name, publicity, _params, _type) = alias
    case publicity {
      g.Public -> option.Some(name)
      g.Private -> option.None
    }
  })
}

fn public_custom_type_names(module: g.Module) -> List(String) {
  let g.Module(_imports, custom_types, _aliases, _constants, _functions) =
    module
  filter_map_option(custom_types, fn(defn) {
    let g.Definition(_attrs, custom_type) = defn
    let g.CustomType(_span, name, publicity, _opaque, _params, _variants) =
      custom_type
    case publicity {
      g.Public -> option.Some(name)
      g.Private -> option.None
    }
  })
}

fn module_type_name_set(module: g.Module) -> set.Set(String) {
  let g.Module(_imports, custom_types, type_aliases, _constants, _functions) =
    module
  let custom_names =
    list.map(custom_types, fn(defn) {
      let g.Definition(_attrs, custom_type) = defn
      let g.CustomType(_span, name, _publicity, _opaque, _params, _variants) =
        custom_type
      name
    })
  let alias_names =
    list.map(type_aliases, fn(defn) {
      let g.Definition(_attrs, alias) = defn
      let g.TypeAlias(_span, name, _publicity, _params, _type) = alias
      name
    })
  set.from_list(list.append(custom_names, alias_names))
}

fn qualify_scheme(
  scheme_: scheme.Scheme,
  module_type_names: set.Set(String),
  prefix: String,
) -> scheme.Scheme {
  let scheme.Forall(vbls, type_) = scheme_
  scheme.Forall(vbls, qualify_type(type_, module_type_names, prefix))
}

fn qualify_tcon(
  tcon: #(List(String), List(#(option.Option(String), types.Type)), types.Type),
  module_type_names: set.Set(String),
  prefix: String,
) -> #(List(String), List(#(option.Option(String), types.Type)), types.Type) {
  let #(free, fields, result) = tcon
  let updated_fields =
    list.map(fields, fn(field) {
      let #(label, field_type) = field
      #(label, qualify_type(field_type, module_type_names, prefix))
    })
  let updated_result = qualify_type(result, module_type_names, prefix)
  #(free, updated_fields, updated_result)
}

fn qualify_type(
  type_: types.Type,
  module_type_names: set.Set(String),
  prefix: String,
) -> types.Type {
  case type_ {
    types.Tvar(_, _) -> type_
    types.Tcon(name, span) ->
      case set.contains(module_type_names, name) {
        True -> types.Tcon(prefix <> name, span)
        False -> type_
      }
    types.Tapp(target, args, span) ->
      types.Tapp(
        qualify_type(target, module_type_names, prefix),
        list.map(args, fn(arg) { qualify_type(arg, module_type_names, prefix) }),
        span,
      )
    types.Ttuple(args, span) ->
      types.Ttuple(
        list.map(args, fn(arg) { qualify_type(arg, module_type_names, prefix) }),
        span,
      )
    types.Tfn(args, result, span) ->
      types.Tfn(
        list.map(args, fn(arg) { qualify_type(arg, module_type_names, prefix) }),
        qualify_type(result, module_type_names, prefix),
        span,
      )
  }
}

fn sort_pairs(items: List(#(String, String))) -> List(#(String, String)) {
  list.sort(items, by: fn(a, b) {
    let #(name_a, _) = a
    let #(name_b, _) = b
    string.compare(name_a, name_b)
  })
}

pub fn infer_module_test() {
  assert Ok(sort_pairs([#("repeat", "fn(Int, a) -> List(a)")]))
    == assert_module_infer(
      "pub fn repeat(i, x) {\n           case i {\n             0 -> []\n             i -> [x, .. repeat(i - 1, x)]\n           }\n         }",
    )
}

pub fn infer_module_test1_test() {
  assert Ok(sort_pairs([#("length", "fn(List(a)) -> Int")]))
    == assert_module_infer(
      "pub fn length(list) {\n           case list {\n           [] -> 0\n           [x, .. xs] -> length(xs) + 1\n           }\n        }",
    )
}

pub fn infer_module_test2_test() {
  assert Ok(
      sort_pairs([
        #("public", "fn() -> Int"),
      ]),
    )
    == assert_module_infer("fn private() { 1 }\n         pub fn public() { 1 }")
}

pub fn infer_module_test3_test() {
  assert Ok(
      sort_pairs([
        #("No", "Is"),
        #("Yes", "Is"),
        #("no", "fn() -> Is"),
        #("yes", "fn() -> Is"),
      ]),
    )
    == assert_module_infer(
      "pub type Is { Yes No }\n         pub fn yes() { Yes }\n         pub fn no() { No }",
    )
}

pub fn infer_module_test4_test() {
  assert Ok(sort_pairs([#("I", "fn(Int) -> Num"), #("one", "fn() -> Num")]))
    == assert_module_infer(
      "pub type Num { I(Int) }\n         pub fn one() { I(1) }",
    )
}

pub fn infer_module_test5_test() {
  assert Ok(
      sort_pairs([
        #("Box", "fn(a) -> Box(a)"),
        #("float", "fn() -> Box(Float)"),
        #("int", "fn() -> Box(Int)"),
      ]),
    )
    == assert_module_infer(
      "pub type Box(a) { Box(a) }\n        pub fn int() { Box(1) }\n        pub fn float() { Box(1.0) }",
    )
}

pub fn infer_module_test6_test() {
  assert Ok(
      sort_pairs([#("Singleton", "Singleton"), #("go", "fn(Singleton) -> Int")]),
    )
    == assert_module_infer(
      "pub type Singleton { Singleton }\n        pub fn go(x) { let Singleton = x 1 }",
    )
}

pub fn infer_module_test7_test() {
  assert Ok(
      sort_pairs([#("Box", "fn(a) -> Box(a)"), #("unbox", "fn(Box(a)) -> a")]),
    )
    == assert_module_infer(
      "pub type Box(a) { Box(a) }\n        pub fn unbox(x) { let Box(a) = x a }",
    )
}

pub fn infer_module_test8_test() {
  assert Ok(sort_pairs([#("I", "fn(Int) -> I"), #("open", "fn(I) -> Int")]))
    == assert_module_infer(
      "pub type I { I(Int) }\n        pub fn open(x) { case x { I(i) -> i  } }",
    )
}

pub fn infer_module_test9_test() {
  assert Ok(
      sort_pairs([
        #("list_of", "fn(a) -> List(a)"),
        #("status", "fn() -> Int"),
      ]),
    )
    == assert_module_infer("pub fn status() { 1 } pub fn list_of(x) { [x] }")
}

pub fn infer_module_test10_test() {
  assert Ok(sort_pairs([#("go", "fn(String) -> String")]))
    == assert_module_infer(
      "\n@external(erlang, \"\", \"\")\npub fn go(x: String) -> String\n",
    )
}

pub fn infer_module_test11_test() {
  assert Ok(sort_pairs([#("go", "fn(Int) -> Float")]))
    == assert_module_infer(
      "\n@external(erlang, \"\", \"\")\npub fn go(x: Int) -> Float\n",
    )
}

pub fn infer_module_test12_test() {
  assert Ok(sort_pairs([#("go", "fn(Int) -> Int")]))
    == assert_module_infer(
      "\n@external(erlang, \"\", \"\")\npub fn go(x: Int) -> Int\n",
    )
}

pub fn infer_module_test13_test() {
  assert Ok(sort_pairs([#("ok", "fn() -> fn(Int) -> Int")]))
    == assert_module_infer(
      "\n@external(erlang, \"\", \"\")\npub fn ok() -> fn(Int) -> Int\n",
    )
}

pub fn infer_module_test14_test() {
  assert Ok(sort_pairs([#("go", "fn(Int) -> a")]))
    == assert_module_infer(
      "\n@external(erlang, \"\", \"\")\npub fn go(x: Int) -> b\n",
    )
}

pub fn infer_module_test15_test() {
  assert Ok(sort_pairs([#("go", "fn(Bool) -> a")]))
    == assert_module_infer(
      "\n@external(erlang, \"\", \"\")\npub fn go(x: Bool) -> b\n",
    )
}

pub fn infer_module_test16_test() {
  assert Ok(sort_pairs([#("go", "fn(List(a)) -> a")]))
    == assert_module_infer(
      "\n@external(erlang, \"\", \"\")\npub fn go(x: List(a)) -> a\n",
    )
}

pub fn infer_module_test17_test() {
  assert Ok(sort_pairs([#("x", "fn() -> a")]))
    == assert_module_infer(
      "\n@external(erlang, \"\", \"\")\nfn go(x: Int) -> b\n        pub fn x() { go(1) }",
    )
}

pub fn infer_module_test18_test() {
  assert Ok(
      sort_pairs([
        #("a", "fn() -> Int"),
        #("b", "fn() -> Float"),
        #("i", "fn(a) -> a"),
      ]),
    )
    == assert_module_infer(
      "@external(erlang, \"\", \"\")\n        fn id(a: a) -> a\n        pub fn i(x) { id(x) }\n        pub fn a() { id(1) }\n        pub fn b() { id(1.0) }",
    )
}

pub fn infer_module_test19_test() {
  assert Ok(sort_pairs([#("len", "fn(List(a)) -> Int")]))
    == assert_module_infer(
      "\n@external(erlang, \"\", \"\")\npub fn len(a: List(a)) -> Int\n",
    )
}

pub fn infer_module_test20_test() {
  assert Ok(sort_pairs([#("is_open", "fn(Connection) -> Bool")]))
    == assert_module_infer(
      "pub type Connection\n\n@external(erlang, \"\", \"\")\npub fn is_open(x: Connection) -> Bool\n",
    )
}

pub fn infer_module_test21_test() {
  assert Ok(sort_pairs([#("pair", "fn(a) -> Pair(a, a)")]))
    == assert_module_infer(
      "pub type Pair(a, b)\n\n@external(erlang, \"\", \"\")\npub fn pair(x: a) -> Pair(a, a)\n",
    )
}

pub fn infer_module_test22_test() {
  assert Ok(
      sort_pairs([
        #("one", "fn() -> Int"),
        #("two", "fn() -> Int"),
        #("zero", "fn() -> Int"),
      ]),
    )
    == assert_module_infer(
      "pub fn one() { 1 }\n         pub fn zero() { one() - 1 }\n         pub fn two() { one() + zero() }",
    )
}

pub fn infer_module_test23_test() {
  assert Ok(
      sort_pairs([
        #("one", "fn() -> Int"),
        #("two", "fn() -> Int"),
        #("zero", "fn() -> Int"),
      ]),
    )
    == assert_module_infer(
      "pub fn one() { 1 }\n         pub fn zero() { one() - 1 }\n         pub fn two() { one() + zero() }",
    )
}

pub fn infer_module_test24_test() {
  assert Ok(
      sort_pairs([
        #("Box", "fn(Int) -> Box"),
      ]),
    )
    == assert_module_infer("pub type Box { Box(boxed: Int) }")
}

pub fn infer_module_test25_test() {
  assert Ok(
      sort_pairs([
        #("Tup", "fn(a, b) -> Tup(a, b)"),
      ]),
    )
    == assert_module_infer("pub type Tup(a, b) { Tup(first: a, second: b) }")
}

pub fn infer_module_test26_test() {
  assert Ok(
      sort_pairs([
        #("Tup", "fn(a, b, c) -> Tup(a, b, c)"),
        #("third", "fn(Tup(a, b, c)) -> c"),
      ]),
    )
    == assert_module_infer(
      "pub type Tup(a, b, c) { Tup(first: a, second: b, third: c) }\n         pub fn third(t) { let Tup(_ , _, third: a) = t a }",
    )
}

pub fn infer_label_shorthand_pattern_test() {
  assert Ok(
      sort_pairs([
        #("Tup", "fn(a, b, c) -> Tup(a, b, c)"),
        #("third", "fn(Tup(a, b, c)) -> c"),
      ]),
    )
    == assert_module_infer(
      "pub type Tup(a, b, c) { Tup(first: a, second: b, third: c) }\n         pub fn third(t) { let Tup(_, _, third:) = t third }",
    )
}

pub fn infer_module_test27_test() {
  assert Ok(
      sort_pairs([
        #("ok", "fn(a) -> #(Int, a)"),
      ]),
    )
    == assert_module_infer("pub fn ok(x) { #(1, x) }")
}

pub fn infer_module_test28_test() {
  assert Ok(sort_pairs([#("ok", "fn(Int) -> #(Int, Int)")]))
    == assert_module_infer(
      "\n@external(erlang, \"\", \"\")\npub fn ok(a: Int) -> #(Int, Int)\n",
    )
}

pub fn infer_module_test29_test() {
  assert Ok(sort_pairs([#("go", "fn(#(a, b)) -> b")]))
    == assert_module_infer(
      "\n@external(erlang, \"\", \"\")\npub fn go(a: #(a, c)) -> c\n",
    )
}

pub fn infer_module_test30_test() {
  assert Ok(
      sort_pairs([
        #("always", "fn(a, b) -> b"),
      ]),
    )
    == assert_module_infer("pub fn always(ignore _a, return b) { b }")
}

pub fn infer_module_test31_test() {
  assert Ok(
      sort_pairs([
        #("I", "fn(Num) -> I"),
        #("Num", "Num"),
      ]),
    )
    == assert_module_infer("pub type I { I(Num) } pub type Num { Num }")
}

pub fn infer_module_test32_test() {
  assert Ok(
      sort_pairs([
        #("I", "fn(Num) -> I"),
      ]),
    )
    == assert_module_infer("pub type I { I(Num) } pub type Num")
}

pub fn type_alias_test() {
  assert Ok(
      sort_pairs([
        #("go", "fn() -> Int"),
      ]),
    )
    == assert_module_infer("type Html = String\n         pub fn go() { 1 }")
  assert Ok(sort_pairs([#("ok_one", "fn() -> Result(Int, String)")]))
    == assert_module_infer(
      "type IntString = Result(Int, String)\n         pub fn ok_one() -> IntString { Ok(1) }",
    )
}

pub fn build_in_type_alias_shadow_test() {
  assert Ok(sort_pairs([#("ok_one", "fn() -> Float")]))
    == assert_module_infer(
      "type Int = Float\n         pub fn ok_one() -> Int { 1.0 }",
    )
}

pub fn accessor_test() {
  assert Ok(
      sort_pairs([
        #("Person", "fn(String, Int) -> Person"),
        #("get_age", "fn(Person) -> Int"),
        #("get_name", "fn(Person) -> String"),
      ]),
    )
    == assert_module_infer(
      "\npub type Person { Person(name: String, age: Int) }\npub fn get_age(person: Person) { person.age }\npub fn get_name(person: Person) { person.name }",
    )
  assert Ok(
      sort_pairs([
        #("One", "fn(String) -> One"),
        #("Two", "fn(One) -> Two"),
        #("get", "fn(Two) -> String"),
      ]),
    )
    == assert_module_infer(
      "\npub type One { One(name: String) }\npub type Two { Two(one: One) }\npub fn get(x: Two) { x.one.name }",
    )
}

pub fn generic_accessor_test() {
  assert Ok(
      sort_pairs([
        #("Box", "fn(a) -> Box(a)"),
        #("get_box", "fn(Box(Box(a))) -> Box(a)"),
        #("get_generic", "fn(Box(a)) -> a"),
        #("get_get_box", "fn(Box(Box(a))) -> a"),
        #("get_int", "fn(Box(Int)) -> Int"),
        #("get_string", "fn(Box(String)) -> String"),
      ]),
    )
    == assert_module_infer(
      "\npub type Box(a) { Box(inner: a) }\npub fn get_box(x: Box(Box(a))) { x.inner }\npub fn get_generic(x: Box(a)) { x.inner }\npub fn get_get_box(x: Box(Box(a))) { x.inner.inner }\npub fn get_int(x: Box(Int)) { x.inner }\npub fn get_string(x: Box(String)) { x.inner }\n",
    )
}

pub fn generic_accessor_later_defined_test() {
  assert Ok(sort_pairs([#("name", "fn(Cat) -> String")]))
    == assert_module_infer(
      "\npub fn name(cat: Cat) {\n  cat.name\n}\n\npub opaque type Cat {\n  Cat(name: String)\n}",
    )
}

pub fn custom_type_annotation_test() {
  assert Ok(
      sort_pairs([
        #("Person", "fn(String, Int) -> Person"),
        #("create_person", "fn(String) -> Person"),
      ]),
    )
    == assert_module_infer(
      "\n        pub type Person {\n            Person(name: String, age: Int)\n        }\n\n        pub fn create_person(name: String) {\n            let x: Person = Person(name: name, age: 1)\n            x\n        }",
    )
  assert Ok(
      sort_pairs([
        #("Box", "fn(a) -> Box(a)"),
        #("create_float_box", "fn(Float) -> Box(Float)"),
        #("create_int_box", "fn(Int) -> Box(Int)"),
      ]),
    )
    == assert_module_infer(
      "\n        pub type Box(inner) {\n            Box(inner)\n        }\n\n        pub fn create_int_box(value: Int) {\n            let x: Box(Int) = Box(value)\n            x\n        }\n\n        pub fn create_float_box(value: Float) {\n            let x: Box(Float) = Box(value)\n            x\n        }",
    )
}

pub fn opaque_accessors_test() {
  assert Ok(sort_pairs([#("get", "fn(One) -> String")]))
    == assert_module_infer(
      "\npub opaque type One { One(name: String) }\npub fn get(x: One) { x.name }",
    )
}

pub fn fn_annotation_reused_test() {
  assert Ok(
      sort_pairs([
        #("Box", "fn(a) -> Box(a)"),
        #("go", "fn(Box(a)) -> fn(Box(a)) -> Bool"),
      ]),
    )
    == assert_module_infer(
      "\n        pub type Box(a) {\n            Box(value: a)\n        }\n        pub fn go(box1: Box(a)) {\n            fn(box2: Box(a)) { box1.value == box2.value }\n        }",
    )
  assert Ok(
      sort_pairs([
        #("Box", "fn(a) -> Box(a)"),
        #("go", "fn(Box(a)) -> fn(Box(a)) -> Bool"),
      ]),
    )
    == assert_module_infer(
      "\n        pub type Box(a) {\n            Box(value: a)\n        }\n        pub fn go(box1: Box(a)) {\n            let x: Box(a) = box1\n            fn(box2: Box(a)) { x.value == box2.value }\n        }",
    )
}

pub fn accessor_multiple_variants_test() {
  assert Ok(
      sort_pairs([
        #("Student", "fn(String, Int) -> Person"),
        #("Teacher", "fn(String, String) -> Person"),
        #("get_name", "fn(Person) -> String"),
      ]),
    )
    == assert_module_infer(
      "\npub type Person {\n    Teacher(name: String, title: String)\n    Student(name: String, age: Int)\n}\npub fn get_name(person: Person) { person.name }",
    )
}

pub fn record_accessor_multiple_variants_parameterised_types_test() {
  assert Ok(
      sort_pairs([
        #("Student", "fn(String, List(Int)) -> Person"),
        #("Teacher", "fn(String, List(Int), String) -> Person"),
        #("get_age", "fn(Person) -> List(Int)"),
        #("get_name", "fn(Person) -> String"),
      ]),
    )
    == assert_module_infer(
      "\npub type Person {\n    Teacher(name: String, age: List(Int), title: String)\n    Student(name: String, age: List(Int))\n}\npub fn get_name(person: Person) { person.name }\npub fn get_age(person: Person) { person.age }",
    )
}

pub fn accessor_multiple_variants_positions_other_than_first_test() {
  assert Ok(
      sort_pairs([
        #("Student", "fn(String, Int) -> Person"),
        #("Teacher", "fn(String, Int, String) -> Person"),
        #("get_age", "fn(Person) -> Int"),
        #("get_name", "fn(Person) -> String"),
      ]),
    )
    == assert_module_infer(
      "\npub type Person {\n    Teacher(name: String, age: Int, title: String)\n    Student(name: String, age: Int)\n}\npub fn get_name(person: Person) { person.name }\npub fn get_age(person: Person) { person.age }",
    )
}

pub fn box_record_test() {
  assert Ok(
      sort_pairs([
        #("Box", "fn(Nil, Int, Int, Int) -> Box"),
        #("main", "fn() -> Box"),
      ]),
    )
    == assert_module_infer(
      "\npub type Box {\n  Box(a: Nil, b: Int, c: Int, d: Int)\n}\n\npub fn main() {\n  Box(b: 1, c: 1, d: 1, a: Nil)\n}",
    )
}

pub fn record_update_no_fields_test() {
  assert Ok(
      sort_pairs([
        #("Person", "fn(String, Int) -> Person"),
        #("identity", "fn(Person) -> Person"),
      ]),
    )
    == assert_module_infer(
      "\n        pub type Person {\n            Person(name: String, age: Int)\n        }\n        pub fn identity(person: Person) {\n            Person(..person)\n        }",
    )
}

pub fn record_update_test() {
  assert Ok(
      sort_pairs([
        #("Person", "fn(String, Int) -> Person"),
        #("update_name", "fn(Person, String) -> Person"),
      ]),
    )
    == assert_module_infer(
      "\n        pub type Person {\n            Person(name: String, age: Int)\n        }\n        pub fn update_name(person: Person, name: String) {\n            Person(..person, name: name)\n        }",
    )
}

pub fn record_update_all_fields_test() {
  assert Ok(
      sort_pairs([
        #("Person", "fn(String, Int) -> Person"),
        #("update_person", "fn(Person, String, Int) -> Person"),
      ]),
    )
    == assert_module_infer(
      "\n        pub type Person {\n            Person(name: String, age: Int)\n        }\n        pub fn update_person(person: Person, name: String, age: Int) {\n            Person(..person, name: name, age: age, )\n        }",
    )
}

pub fn record_update_out_of_order_test() {
  assert Ok(
      sort_pairs([
        #("Person", "fn(String, Int) -> Person"),
        #("update_person", "fn(Person, String, Int) -> Person"),
      ]),
    )
    == assert_module_infer(
      "\n        pub type Person {\n            Person(name: String, age: Int)\n        }\n        pub fn update_person(person: Person, name: String, age: Int) {\n            Person(..person, age: age, name: name)\n        }",
    )
}

pub fn record_update_generic_test() {
  assert Ok(
      sort_pairs([
        #("Box", "fn(a, b) -> Box(a, b)"),
        #(
          "combine_boxes",
          "fn(Box(Int, Bool), Box(Bool, Int)) -> Box(Int, Bool)",
        ),
      ]),
    )
    == assert_module_infer(
      "\n        pub type Box(a, b) {\n            Box(left: a, right: b)\n        }\n\n        pub fn combine_boxes(a: Box(Int, Bool), b: Box(Bool, Int)) {\n            Box(..a, left: a.left + b.right, right: b.left)\n        }",
    )
}

pub fn record_update_generic_unannotated_test() {
  assert Ok(
      sort_pairs([
        #("Box", "fn(a, b) -> Box(a, b)"),
        #("combine_boxes", "fn(Box(a, b), Box(b, a)) -> Box(a, b)"),
      ]),
    )
    == assert_module_infer(
      "\n        pub type Box(a, b) {\n            Box(left: a, right: b)\n        }\n\n        pub fn combine_boxes(a: Box(t1, t2), b: Box(t2, t1)) {\n            Box(..a, left: b.right, right: b.left)\n        }",
    )
}

pub fn record_update_variant_inference_test() {
  assert Ok(
      sort_pairs([
        #("Circle", "fn(Int, Int, Int) -> Shape"),
        #("Square", "fn(Int, Int, Int, Int) -> Shape"),
        #("grow", "fn(Shape) -> Shape"),
      ]),
    )
    == assert_module_infer(
      "\npub type Shape {\n  Circle(cx: Int, cy: Int, radius: Int)\n  Square(x: Int, y: Int, width: Int, height: Int)\n}\n\npub fn grow(shape) {\n  case shape {\n    Circle(radius:, ..) as circle -> Circle(..circle, radius: radius + 1)\n    Square(width:, height:, ..) as square -> Square(..square, width: width + 1, height: height + 1)\n  }\n}\n",
    )
}

pub fn record_access_variant_inference_test() {
  assert Ok(
      sort_pairs([
        #("Wibble", "fn(Int, Int) -> Wibble"),
        #("Wobble", "fn(Int, Int) -> Wibble"),
        #("get", "fn(Wibble) -> Int"),
      ]),
    )
    == assert_module_infer(
      "\npub type Wibble {\n  Wibble(a: Int, b: Int)\n  Wobble(a: Int, c: Int)\n}\n\npub fn get(wibble) {\n  case wibble {\n    Wibble(..) as w -> w.b\n    Wobble(..) as w -> w.c\n  }\n}\n",
    )
}

pub fn local_variable_variant_inference_test() {
  assert Ok(
      sort_pairs([
        #("Wibble", "fn(Int, Int) -> Wibble"),
        #("Wobble", "fn(Int, Int) -> Wibble"),
        #("main", "fn() -> Int"),
      ]),
    )
    == assert_module_infer(
      "\npub type Wibble {\n  Wibble(a: Int, b: Int)\n  Wobble(a: Int, c: Int)\n}\n\npub fn main() {\n  let always_wibble = Wibble(1, 2)\n  always_wibble.b\n}\n",
    )
}

pub fn record_update_variant_inference_for_original_variable_test() {
  assert Ok(
      sort_pairs([
        #("Wibble", "fn(Int, Int) -> Wibble"),
        #("Wobble", "fn(Int, String) -> Wibble"),
        #("update", "fn(Wibble) -> Wibble"),
      ]),
    )
    == assert_module_infer(
      "\npub type Wibble {\n  Wibble(a: Int, b: Int)\n  Wobble(a: Int, c: String)\n}\n\npub fn update(wibble: Wibble) -> Wibble {\n  case wibble {\n    Wibble(..) -> Wibble(..wibble, a: 1)\n    Wobble(..) -> Wobble(..wibble, c: \"hello\")\n  }\n}\n",
    )
}

pub fn record_access_variant_inference_for_original_variable_test() {
  assert Ok(
      sort_pairs([
        #("Wibble", "fn(Int, Int) -> Wibble"),
        #("Wobble", "fn(Int, Int) -> Wibble"),
        #("get", "fn(Wibble) -> Int"),
      ]),
    )
    == assert_module_infer(
      "\npub type Wibble {\n  Wibble(a: Int, b: Int)\n  Wobble(a: Int, c: Int)\n}\n\npub fn get(wibble) {\n  case wibble {\n    Wibble(..) -> wibble.b\n    Wobble(..) -> wibble.c\n  }\n}\n",
    )
}

pub fn variant_inference_for_imported_type_test() {
  assert Ok(sort_pairs([#("main", "fn(Wibble) -> Wibble")]))
    == assert_module_infer_with_deps(
      [
        #(
          "wibble",
          "\npub type Wibble {\n  Wibble(a: Int, b: Int)\n  Wobble(a: Int, c: Int)\n}\n",
        ),
      ],
      "\nimport wibble.{Wibble, Wobble}\n\npub fn main(wibble) {\n  case wibble {\n    Wibble(..) -> Wibble(a: 1, b: wibble.b + 1)\n    Wobble(..) -> Wobble(..wibble, c: wibble.c - 4)\n  }\n}\n",
    )
}

pub fn local_variable_variant_inference_for_imported_type_test() {
  assert Ok(sort_pairs([#("main", "fn() -> Wibble")]))
    == assert_module_infer_with_deps(
      [
        #(
          "wibble",
          "\npub type Wibble {\n  Wibble(a: Int, b: Int)\n  Wobble(a: Int, c: Int)\n}\n",
        ),
      ],
      "\nimport wibble.{Wibble}\n\npub fn main() {\n  let wibble = Wibble(4, 9)\n  Wibble(..wibble, b: wibble.b)\n}\n",
    )
}

pub fn record_update_variant_inference_in_alternate_pattern_with_all_same_variants_test() {
  assert Ok(
      sort_pairs([
        #("Vector2", "fn(Float, Float) -> Vector"),
        #("Vector3", "fn(Float, Float, Float) -> Vector"),
        #("increase_y", "fn(Vector, Float) -> Vector"),
      ]),
    )
    == assert_module_infer(
      "\npub type Vector {\n  Vector2(x: Float, y: Float)\n  Vector3(x: Float, y: Float, z: Float)\n}\n\npub fn increase_y(vector, by increase) {\n  case vector {\n    Vector2(y:, ..) as vector -> Vector2(..vector, y: y +. increase)\n    Vector3(y:, z: 12.3, ..) as vector | Vector3(y:, z: 15.0, ..) as vector ->\n      Vector3(..vector, y: y +. increase, z: 0.0)\n    _ -> panic as \"Could not increase Y\"\n  }\n}\n",
    )
}

pub fn module_constants_test() {
  assert Ok(
      sort_pairs([
        #("test_float", "Float"),
        #("test_int1", "Int"),
        #("test_int2", "Int"),
        #("test_int3", "Int"),
        #("test_int4", "Int"),
        #("test_int5", "Int"),
        #("test_list", "List(Int)"),
        #("test_string", "String"),
        #("test_tuple", "#(String, Int)"),
        #("test_var1", "Int"),
        #("test_var2", "Int"),
      ]),
    )
    == assert_module_infer(
      "\n    pub const test_int1 = 123\n    pub const test_int2: Int = 321\n    pub const test_int3 = 0xE\n    pub const test_int4 = 0o10\n    pub const test_int5 = 0o10011\n    pub const test_float: Float = 4.2\n    pub const test_string = \"hey!\"\n    pub const test_list = [1,2,3]\n    pub const test_tuple = #(\"yes!\", 42)\n    pub const test_var1 = test_int1\n    pub const test_var2: Int = test_int1",
    )
}

pub fn custom_type_module_constants_test() {
  assert Ok(
      sort_pairs([
        #("A", "Test"),
        #("some_test", "Test"),
      ]),
    )
    == assert_module_infer(
      "pub type Test { A }\n        pub const some_test = A",
    )
}

pub fn const_record_update_test() {
  assert Ok(
      sort_pairs([
        #("Person", "fn(String, Int) -> Person"),
        #("alice", "Person"),
        #("bob", "Person"),
      ]),
    )
    == assert_module_infer(
      "pub type Person { Person(name: String, age: Int) }\n\n        pub const alice = Person(\"Alice\", 30)\n        pub const bob = Person(..alice, name: \"Bob\")",
    )
}

pub fn const_record_update_chain_test() {
  assert Ok(
      sort_pairs([
        #("Person", "fn(String, Int, String) -> Person"),
        #("alice", "Person"),
        #("bob", "Person"),
        #("charlie", "Person"),
      ]),
    )
    == assert_module_infer(
      "pub type Person { Person(name: String, age: Int, city: String) }\n\n        pub const alice = Person(\"Alice\", 30, \"London\")\n        pub const bob = Person(..alice, name: \"Bob\")\n        pub const charlie = Person(..bob, age: 25)",
    )
}

pub fn const_record_update_with_labeled_args_test() {
  assert Ok(
      sort_pairs([
        #("Person", "fn(String, Int, String) -> Person"),
        #("alice", "Person"),
        #("bob", "Person"),
      ]),
    )
    == assert_module_infer(
      "pub type Person { Person(name: String, age: Int, city: String) }\n        pub const alice = Person(name: \"Alice\", age: 30, city: \"London\")\n        pub const bob = Person(..alice, name: \"Bob\")",
    )
}

pub fn const_record_update_multi_variant_test() {
  assert Ok(
      sort_pairs([
        #("Cat", "fn(String, String) -> Pet"),
        #("Dog", "fn(String, Int) -> Pet"),
        #("another_dog", "Pet"),
        #("my_dog", "Pet"),
      ]),
    )
    == assert_module_infer(
      "pub type Pet {\n          Dog(name: String, age: Int)\n          Cat(name: String, breed: String)\n        }\n\n        pub const my_dog = Dog(\"Rex\", 5)\n        pub const another_dog = Dog(..my_dog, name: \"Max\")",
    )
}

pub fn const_record_update_generic_respecialization_test() {
  assert Ok(
      sort_pairs([
        #("Box", "fn(String, a) -> Box(a)"),
        #("base", "Box(Int)"),
        #("updated", "Box(String)"),
      ]),
    )
    == assert_module_infer(
      "pub type Box(a) {\n            Box(name: String, value: a)\n        }\n\n        pub const base = Box(\"score\", 50)\n        pub const updated = Box(..base, value: \"Hello\")",
    )
}

pub fn module_constant_functions_test() {
  assert Ok(
      sort_pairs([
        #("int_identity", "fn(Int) -> Int"),
        #("int_identity_alias1", "fn(Int) -> Int"),
        #("int_identity_alias2", "fn(Int) -> Int"),
        #("int_identity_alias3", "fn(Int) -> Int"),
      ]),
    )
    == assert_module_infer(
      "pub fn int_identity(i: Int) -> Int { i }\n        pub const int_identity_alias1 = int_identity\n        pub const int_identity_alias2 = int_identity_alias1\n        pub const int_identity_alias3: fn(Int) -> Int = int_identity_alias2",
    )
}

pub fn functions_used_before_definition_test() {
  assert Ok(
      sort_pairs([
        #("a", "fn() -> Int"),
        #("b", "fn() -> Int"),
      ]),
    )
    == assert_module_infer("pub fn a() { b() }\n         pub fn b() { 1 }")
}

pub fn functions_used_before_definition1_test() {
  assert Ok(sort_pairs([#("a", "fn() -> Int")]))
    == assert_module_infer(
      "pub fn a() { b() + c() }\n         fn b() { 1 }\n         fn c() { 1 }",
    )
}

pub fn functions_used_before_definition2_test() {
  assert Ok(sort_pairs([#("a", "fn() -> Int")]))
    == assert_module_infer(
      "fn b() { 1 }\n         pub fn a() { b() + c() }\n         fn c() { 1 }",
    )
}

pub fn functions_used_before_definition3_test() {
  assert Ok(sort_pairs([#("Thing", "Thing"), #("a", "fn() -> Thing")]))
    == assert_module_infer(
      "pub fn a() { Thing }\n         pub type Thing { Thing }",
    )
}

pub fn types_used_before_definition_test() {
  assert Ok(
      sort_pairs([
        #("Y", "fn(X) -> Y"),
      ]),
    )
    == assert_module_infer("pub type Y { Y(X) }\n         pub type X")
}

pub fn types_used_before_definition1_test() {
  assert Ok(
      sort_pairs([
        #("Y", "fn(X) -> Y"),
      ]),
    )
    == assert_module_infer("pub type Y { Y(x: X) }\n         pub type X")
}

pub fn consts_used_before_definition_test() {
  assert Ok(
      sort_pairs([
        #("a", "fn() -> Int"),
      ]),
    )
    == assert_module_infer("pub fn a() { b }\n        const b = 1")
}

pub fn mutual_recursion_test() {
  assert Ok(
      sort_pairs([
        #("a", "fn() -> a"),
      ]),
    )
    == assert_module_infer("pub fn a() { b() }\n         fn b() { a() }")
  assert Ok(
      sort_pairs([
        #("a", "fn() -> Int"),
      ]),
    )
    == assert_module_infer("pub fn a() { b() }\n         fn b() { a() + 1 }")
}

pub fn type_annotations_test() {
  assert Ok(
      sort_pairs([
        #("Box", "fn(String, a) -> Box(a)"),
        #("id", "fn(Box(a)) -> Box(a)"),
      ]),
    )
    == assert_module_infer(
      "pub type Box(x) { Box(label: String, contents: x) }\n         pub fn id(x: Box(y)) { x }",
    )
  assert Ok(sort_pairs([#("go", "fn(Int) -> Int")]))
    == assert_module_infer("pub fn go(x: Int) { x }")
  assert Ok(sort_pairs([#("go", "fn(a) -> a")]))
    == assert_module_infer("pub fn go(x: b) -> b { x }")
  assert Ok(sort_pairs([#("go", "fn(a) -> a")]))
    == assert_module_infer("pub fn go(x) -> b { x }")
  assert Ok(sort_pairs([#("go", "fn(a) -> a")]))
    == assert_module_infer("pub fn go(x: b) { x }")
  assert Ok(
      sort_pairs([
        #("go", "fn(List(a)) -> List(a)"),
      ]),
    )
    == assert_module_infer("pub fn go(x: List(b)) -> List(b) { x }")
  assert Ok(
      sort_pairs([
        #("go", "fn(List(a)) -> List(a)"),
      ]),
    )
    == assert_module_infer("pub fn go(x: List(b)) { x }")
  assert Ok(
      sort_pairs([
        #("go", "fn(List(String)) -> List(String)"),
      ]),
    )
    == assert_module_infer("pub fn go(x: List(String)) { x }")
  assert Ok(sort_pairs([#("go", "fn(a, b) -> a")]))
    == assert_module_infer("pub fn go(x: b, y: c) { x }")
  assert Ok(sort_pairs([#("go", "fn(Int) -> Int")]))
    == assert_module_infer("pub fn go(x) -> Int { x }")
  assert Ok(
      sort_pairs([
        #("float", "fn() -> Float"),
        #("id", "fn(a) -> a"),
        #("int", "fn() -> Int"),
      ]),
    )
    == assert_module_infer(
      "pub fn id(x: x) { x }\n         pub fn float() { id(1.0) }\n         pub fn int() { id(1) }",
    )
}

pub fn early_function_generalisation_test() {
  assert Ok(
      sort_pairs([
        #("id", "fn(a) -> a"),
        #("int", "fn() -> Int"),
      ]),
    )
    == assert_module_infer(
      "pub fn id(x) { x }\n         pub fn int() { id(1) }",
    )
}

pub fn early_function_generalisation2_test() {
  assert Ok(
      sort_pairs([
        #("float", "fn() -> Float"),
        #("id", "fn(a) -> a"),
        #("int", "fn() -> Int"),
      ]),
    )
    == assert_module_infer(
      "pub fn id(x) { x }\n         pub fn int() { id(1) }\n         pub fn float() { id(1.0) }\n         ",
    )
}

pub fn bit_array_pattern_unification_test() {
  assert Ok(
      sort_pairs([
        #("m", "fn(BitArray) -> Nil"),
      ]),
    )
    == assert_module_infer("pub fn m(x) { case x { <<>> -> Nil _ -> Nil} }")
}

pub fn bit_array_pattern_unification2_test() {
  assert Ok(
      sort_pairs([
        #("m", "fn(BitArray) -> Nil"),
      ]),
    )
    == assert_module_infer("pub fn m(x) { case x { <<>> -> Nil _ -> Nil} }")
}

pub fn qualified_prelude_test() {
  assert Ok(
      sort_pairs([
        #("a", "fn() -> Result(Int, a)"),
      ]),
    )
    == assert_module_infer("import gleam\npub fn a() {\n  gleam.Ok(1)\n}")
}

pub fn empty_list_const_test() {
  assert Ok(
      sort_pairs([
        #("a", "fn() -> List(a)"),
        #("empty", "List(a)"),
      ]),
    )
    == assert_module_infer("pub const empty = []\npub fn a() {\n    empty\n}")
}

pub fn generic_inner_access_test() {
  assert Ok(
      sort_pairs([#("B", "fn(a) -> B(a)"), #("b_get_first", "fn(B(#(a))) -> a")]),
    )
    == assert_module_infer(
      "pub type B(b) { B(value: b) }\npub fn b_get_first(b: B(#(a))) {\n  b.value.0\n}",
    )
}

pub fn fn_contextual_info_test() {
  assert Ok(sort_pairs([#("main", "fn() -> Int")]))
    == assert_module_infer(
      "\ntype Box {\n  Box(inner: Int)\n}\n\nfn call(argument: t, function: fn(t) -> tt) -> tt {\n  function(argument)\n}\n\npub fn main() {\n  call(Box(1), fn(box) { box.inner })\n}\n",
    )
}

pub fn permit_holes_in_fn_args_and_returns_test() {
  assert Ok(
      sort_pairs([
        #("run", "fn(List(a)) -> List(b)"),
      ]),
    )
    == assert_module_infer("pub fn run(args: List(_)) -> List(_) {\n  todo\n}")
}

pub fn block_maths_test() {
  assert Ok(sort_pairs([#("do", "fn(Float, Float) -> Float")]))
    == assert_module_infer(
      "pub fn do(max, min) {\n  { max -. min } /. { max +. min }\n}",
    )
}

pub fn infer_label_shorthand_in_call_arg_test() {
  assert Ok(
      sort_pairs([
        #("main", "fn() -> Nil"),
        #("wibble", "fn(Int, Float, Bool) -> Nil"),
      ]),
    )
    == assert_module_infer(
      "\n    pub fn main() {\n        let arg1 = 1\n        let arg2 = 1.0\n        let arg3 = False\n        wibble(arg2:, arg3:, arg1:)\n    }\n\n    pub fn wibble(arg1 arg1: Int, arg2 arg2: Float, arg3 arg3: Bool) { Nil }\n        ",
    )
}

pub fn infer_label_shorthand_in_constructor_arg_test() {
  assert Ok(
      sort_pairs([
        #("Wibble", "fn(Int, Bool, Float) -> Wibble"),
        #("main", "fn() -> Wibble"),
      ]),
    )
    == assert_module_infer(
      "\n    pub type Wibble { Wibble(arg1: Int, arg2: Bool, arg3: Float) }\n    pub fn main() {\n        let arg1 = 1\n        let arg2 = True\n        let arg3 = 1.0\n        Wibble(arg2:, arg3:, arg1:)\n    }\n",
    )
}

pub fn infer_label_shorthand_in_constant_constructor_arg_test() {
  assert Ok(
      sort_pairs([
        #("Wibble", "fn(Int, Bool, Float) -> Wibble"),
        #("arg1", "Int"),
        #("arg2", "Bool"),
        #("arg3", "Float"),
        #("wibble", "Wibble"),
      ]),
    )
    == assert_module_infer(
      "\n    pub type Wibble { Wibble(arg1: Int, arg2: Bool, arg3: Float) }\n    pub const arg1 = 1\n    pub const arg2 = True\n    pub const arg3 = 1.0\n\n    pub const wibble = Wibble(arg2:, arg3:, arg1:)\n",
    )
}

pub fn infer_label_shorthand_in_pattern_arg_test() {
  assert Ok(
      sort_pairs([
        #("Wibble", "fn(Int, Bool, Int) -> Wibble"),
        #("main", "fn() -> Int"),
      ]),
    )
    == assert_module_infer(
      "\n    pub type Wibble { Wibble(arg1: Int, arg2: Bool, arg3: Int) }\n    pub fn main() {\n        case Wibble(1, True, 2) {\n           Wibble(arg2:, arg3:, arg1:) if arg2 -> arg1 * arg3\n           _ -> 0\n        }\n    }\n",
    )
}

pub fn infer_label_shorthand_in_record_update_arg_test() {
  assert Ok(
      sort_pairs([
        #("Wibble", "fn(Int, Bool, Float) -> Wibble"),
        #("main", "fn() -> Wibble"),
      ]),
    )
    == assert_module_infer(
      "\n    pub type Wibble { Wibble(arg1: Int, arg2: Bool, arg3: Float) }\n    pub fn main() {\n        let wibble = Wibble(1, True, 2.0)\n        let arg3 = 3.0\n        let arg2 = False\n        Wibble(..wibble, arg3:, arg2:)\n    }\n",
    )
}

pub fn pipe_with_annonymous_unannotated_functions_test() {
  assert Ok(sort_pairs([#("main", "fn() -> Int")]))
    == assert_module_infer(
      "\npub fn main() {\n  let a = 1\n     |> fn (x) { #(x, x + 1) }\n     |> fn (x) { x.0 }\n     |> fn (x) { x }\n}\n",
    )
}

pub fn pipe_with_annonymous_mixed_functions_test() {
  assert Ok(sort_pairs([#("main", "fn() -> String")]))
    == assert_module_infer(
      "\npub fn main() {\n  let a = \"abc\"\n     |> fn (x) { #(x, x <> \"d\") }\n     |> fn (x) { x.0 }\n     |> fn (x: String) { x }\n}\n",
    )
}

pub fn pipe_with_annonymous_functions_using_structs_test() {
  assert Ok(sort_pairs([#("main", "fn() -> Int")]))
    == assert_module_infer(
      "\ntype Date {\n  Date(day: Day)\n}\ntype Day {\n  Day(year: Int)\n}\nfn now() -> Date {\n  Date(Day(2024))\n}\nfn get_day(date: Date) -> Day {\n  date.day\n}\npub fn main() {\n  now() |> get_day() |> fn (it) { it.year }\n}\n",
    )
}

pub fn labelled_argument_ordering_test() {
  assert Ok(sort_pairs([#("main", "fn() -> Nil")]))
    == assert_module_infer(
      "\ntype A { A }\ntype B { B }\ntype C { C }\ntype D { D }\n\nfn wibble(a a: A, b b: B, c c: C, d d: D) {\n  Nil\n}\n\npub fn main() {\n  wibble(A, C, D, b: B)\n  wibble(A, C, D, b: B)\n  wibble(B, C, D, a: A)\n  wibble(B, C, a: A, d: D)\n  wibble(B, C, d: D, a: A)\n  wibble(B, D, a: A, c: C)\n  wibble(B, D, c: C, a: A)\n  wibble(C, D, b: B, a: A)\n}\n",
    )
}

pub fn variant_inference_allows_inference_test() {
  assert Ok(
      sort_pairs([
        #("Wibble", "fn(Int) -> Wibble"),
        #("Wobble", "fn(Int) -> Wibble"),
        #("do_a_thing", "fn(Wibble) -> Wibble"),
      ]),
    )
    == assert_module_infer(
      "\npub type Wibble {\n  Wibble(a: Int)\n  Wobble(b: Int)\n}\n\npub fn do_a_thing(wibble) {\n  case wibble {\n    Wibble(..) -> wibble.a\n    _ -> todo\n  }\n  wibble\n}\n",
    )
}

pub fn variant_inference_allows_inference2_test() {
  assert Ok(
      sort_pairs([
        #("Box", "fn(a) -> Box(a)"),
        #("UnBox", "Box(a)"),
        #("rebox", "fn(Box(Int)) -> Box(Int)"),
      ]),
    )
    == assert_module_infer(
      "\npub type Box(a) {\n  Box(inner: a)\n  UnBox\n}\n\npub fn rebox(box) {\n  case box {\n    Box(..) -> Box(box.inner + 1)\n    UnBox -> UnBox\n  }\n}\n",
    )
}

pub fn variant_inference_on_prelude_types_test() {
  assert Ok(sort_pairs([#("main", "fn() -> Int")]))
    == assert_module_infer(
      "\npub fn main() {\n  let always_ok = Ok(10)\n  case always_ok {\n    Ok(1) -> 1\n    Ok(2) -> 3\n    _ -> panic\n  }\n}\n",
    )
}

pub fn variant_inference_with_let_assert_test() {
  assert Ok(sort_pairs([#("main", "fn() -> Int")]))
    == assert_module_infer(
      "\ntype Wibble {\n  Wibble(n: Int, b: Bool)\n  Wobble\n}\n\npub fn main() {\n  let wibble = wibble()\n  let assert Wibble(..) = wibble\n  wibble.n\n}\n\nfn wibble() {\n  Wibble(1, True)\n}\n",
    )
}

pub fn variant_inference_with_let_assert_and_alias_test() {
  assert Ok(sort_pairs([#("main", "fn() -> Int")]))
    == assert_module_infer(
      "\ntype Wibble {\n  Wibble(n: Int, b: Bool)\n  Wobble\n}\n\npub fn main() {\n  let assert Wibble(..) as wibble = wibble()\n  wibble.n\n}\n\nfn wibble() {\n  Wibble(1, True)\n}\n",
    )
}

pub fn no_stack_overflow_for_nested_use_test() {
  assert Ok(sort_pairs([#("main", "fn() -> Nil")]))
    == assert_module_infer_with_deps(
      [
        #(
          "gleam/dynamic/decode",
          "\npub fn string() {\n  Nil\n}\n\npub fn field(name, decoder, callback) {\n  callback(Nil)\n}\n",
        ),
      ],
      "\nimport gleam/dynamic/decode\n\npub fn main() {\n  use _n1 <- decode.field(\"n1\", decode.string)\n  use _n2 <- decode.field(\"n2\", decode.string)\n  use _n3 <- decode.field(\"n3\", decode.string)\n  use _n4 <- decode.field(\"n4\", decode.string)\n  use _n5 <- decode.field(\"n5\", decode.string)\n  use _n6 <- decode.field(\"n6\", decode.string)\n  use _n7 <- decode.field(\"n7\", decode.string)\n  use _n8 <- decode.field(\"n8\", decode.string)\n  use _n9 <- decode.field(\"n9\", decode.string)\n  use _n10 <- decode.field(\"n10\", decode.string)\n  use _n11 <- decode.field(\"n11\", decode.string)\n  use _n12 <- decode.field(\"n12\", decode.string)\n  use _n13 <- decode.field(\"n13\", decode.string)\n  use _n14 <- decode.field(\"n14\", decode.string)\n  use _n15 <- decode.field(\"n15\", decode.string)\n  use _n16 <- decode.field(\"n16\", decode.string)\n  use _n17 <- decode.field(\"n17\", decode.string)\n  use _n18 <- decode.field(\"n18\", decode.string)\n  use _n19 <- decode.field(\"n19\", decode.string)\n  use _n20 <- decode.field(\"n20\", decode.string)\n  use _n21 <- decode.field(\"n21\", decode.string)\n  use _n22 <- decode.field(\"n22\", decode.string)\n  use _n23 <- decode.field(\"n23\", decode.string)\n  use _n24 <- decode.field(\"n24\", decode.string)\n  use _n25 <- decode.field(\"n25\", decode.string)\n  use _n26 <- decode.field(\"n26\", decode.string)\n  use _n27 <- decode.field(\"n27\", decode.string)\n  use _n28 <- decode.field(\"n28\", decode.string)\n  use _n29 <- decode.field(\"n29\", decode.string)\n  use _n30 <- decode.field(\"n30\", decode.string)\n  use _n31 <- decode.field(\"n31\", decode.string)\n  use _n32 <- decode.field(\"n32\", decode.string)\n  use _n33 <- decode.field(\"n33\", decode.string)\n  use _n34 <- decode.field(\"n34\", decode.string)\n  use _n35 <- decode.field(\"n35\", decode.string)\n  use _n36 <- decode.field(\"n36\", decode.string)\n  use _n37 <- decode.field(\"n37\", decode.string)\n  use _n38 <- decode.field(\"n38\", decode.string)\n  use _n39 <- decode.field(\"n39\", decode.string)\n  use _n40 <- decode.field(\"n40\", decode.string)\n  use _n41 <- decode.field(\"n41\", decode.string)\n  use _n42 <- decode.field(\"n42\", decode.string)\n  use _n43 <- decode.field(\"n43\", decode.string)\n  use _n44 <- decode.field(\"n44\", decode.string)\n  use _n45 <- decode.field(\"n45\", decode.string)\n  use _n46 <- decode.field(\"n46\", decode.string)\n  use _n47 <- decode.field(\"n47\", decode.string)\n  use _n48 <- decode.field(\"n48\", decode.string)\n  use _n49 <- decode.field(\"n49\", decode.string)\n  use _n50 <- decode.field(\"n50\", decode.string)\n  use _n51 <- decode.field(\"n51\", decode.string)\n  use _n52 <- decode.field(\"n52\", decode.string)\n  use _n53 <- decode.field(\"n53\", decode.string)\n  use _n54 <- decode.field(\"n54\", decode.string)\n  use _n55 <- decode.field(\"n55\", decode.string)\n  Nil\n}\n",
    )
}

pub fn variant_inference_ignores_echo_test() {
  assert Ok(
      sort_pairs([
        #("Wibble", "fn(Int, String) -> Wibble"),
        #("Wobble", "fn(String, Int) -> Wibble"),
        #("get_int", "fn(Wibble) -> Int"),
      ]),
    )
    == assert_module_infer(
      "\npub type Wibble {\n  Wibble(wibble: Int, wobble: String)\n  Wobble(a: String, b: Int)\n}\n\npub fn get_int(w: Wibble) {\n  case echo w {\n    Wibble(..) -> w.wibble\n    Wobble(..) -> w.b\n  }\n}\n",
    )
}
