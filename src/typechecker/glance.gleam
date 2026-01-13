import glance as g
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import typechecker/ast
import typechecker/types

pub type Error {
  Unsupported(String)
}

pub fn module_to_tops(module: g.Module) -> Result(List(ast.Top), Error) {
  let g.Module(_imports, custom_types, type_aliases, constants, functions) =
    module
  let results =
    list.flatten([
      list.map(type_aliases, definition_to_type_alias),
      list.map(custom_types, definition_to_custom_type),
      list.map(constants, definition_to_constant),
      list.map(functions, definition_to_function),
    ])

  result.all(results)
}

pub fn expression(expr: g.Expression) -> Result(ast.Expr, Error) {
  case expr {
    g.Int(span, value) ->
      case int.parse(value) {
        Ok(int_value) ->
          Ok(ast.Eprim(
            ast.Pint(int_value, loc_from_span(span)),
            loc_from_span(span),
          ))
        Error(_) -> Error(Unsupported("int literal"))
      }

    g.String(span, value) -> Ok(ast.Estr(value, [], loc_from_span(span)))

    g.Variable(span, name) ->
      case name {
        "True" ->
          Ok(ast.Eprim(
            ast.Pbool(True, loc_from_span(span)),
            loc_from_span(span),
          ))
        "False" ->
          Ok(ast.Eprim(
            ast.Pbool(False, loc_from_span(span)),
            loc_from_span(span),
          ))
        _ -> Ok(ast.Evar(name, loc_from_span(span)))
      }

    g.Tuple(span, elements) ->
      case elements {
        [left, right] ->
          map2(expression(left), expression(right), fn(left_expr, right_expr) {
            ast.Eapp(
              ast.Evar(",", loc_from_span(span)),
              [left_expr, right_expr],
              loc_from_span(span),
            )
          })
        _ -> Error(Unsupported("tuple arity"))
      }

    g.Call(span, function, arguments) ->
      map2(
        expression(function),
        field_arguments(arguments),
        fn(func_expr, args) { ast.Eapp(func_expr, args, loc_from_span(span)) },
      )

    g.Fn(span, arguments, _return_annotation, body) ->
      map2(
        fn_parameters(arguments),
        block_to_expression(body, loc_from_span(span)),
        fn(params, body_expr) {
          ast.Elambda(params, body_expr, loc_from_span(span))
        },
      )

    g.Block(span, statements) ->
      block_to_expression(statements, loc_from_span(span))

    g.BinaryOperator(span, op, left, right) ->
      map2(expression(left), expression(right), fn(left_expr, right_expr) {
        let name = case op {
          g.AddInt -> Ok("+")
          g.SubInt -> Ok("-")
          g.LtInt -> Ok("<")
          g.LtEqInt -> Ok("<=")
          g.GtInt -> Ok(">")
          g.GtEqInt -> Ok(">=")
          g.Eq -> Ok("=")
          g.NotEq -> Ok("!=")
          _ -> Error(Unsupported("binary operator"))
        }

        result.map(name, fn(n) {
          let loc = loc_from_span(span)
          let applied_left = ast.Eapp(ast.Evar(n, loc), [left_expr], loc)
          ast.Eapp(applied_left, [right_expr], loc)
        })
      })
      |> result.flatten

    g.Case(span, subjects, clauses) ->
      case subjects {
        [subject] ->
          map2(
            expression(subject),
            result.all(list.map(clauses, clause_to_case)),
            fn(target_expr, cases) {
              ast.Ematch(target_expr, cases, loc_from_span(span))
            },
          )
        _ -> Error(Unsupported("case subject count"))
      }

    g.NegateInt(_, _)
    | g.NegateBool(_, _)
    | g.Float(_, _)
    | g.Panic(_, _)
    | g.Todo(_, _)
    | g.List(_, _, _)
    | g.RecordUpdate(_, _, _, _, _)
    | g.FieldAccess(_, _, _)
    | g.TupleIndex(_, _, _)
    | g.FnCapture(_, _, _, _, _)
    | g.BitString(_, _)
    | g.Echo(_, _, _) -> Error(Unsupported("expression"))
  }
}

pub fn pattern(pat: g.Pattern) -> Result(ast.Pat, Error) {
  case pat {
    g.PatternVariable(span, name) -> Ok(ast.Pvar(name, loc_from_span(span)))
    g.PatternDiscard(span, _name) -> Ok(ast.Pany(loc_from_span(span)))
    g.PatternInt(span, value) ->
      case int.parse(value) {
        Ok(int_value) ->
          Ok(ast.Pprim(
            ast.Pint(int_value, loc_from_span(span)),
            loc_from_span(span),
          ))
        Error(_) -> Error(Unsupported("int pattern"))
      }
    g.PatternString(span, value) -> Ok(ast.Pstr(value, loc_from_span(span)))

    g.PatternTuple(span, elements) ->
      case elements {
        [left, right] ->
          map2(pattern(left), pattern(right), fn(left_pat, right_pat) {
            ast.Pcon(
              ",",
              loc_from_span(span),
              [left_pat, right_pat],
              loc_from_span(span),
            )
          })
        _ -> Error(Unsupported("tuple pattern arity"))
      }

    g.PatternVariant(span, module, constructor, arguments, with_spread) ->
      case module, with_spread {
        Some(_), _ -> Error(Unsupported("qualified constructor"))
        _, True -> Error(Unsupported("spread pattern"))
        _, False ->
          result.map(field_patterns(arguments), fn(args) {
            ast.Pcon(
              constructor,
              loc_from_span(span),
              args,
              loc_from_span(span),
            )
          })
      }

    g.PatternFloat(_, _)
    | g.PatternList(_, _, _)
    | g.PatternAssignment(_, _, _)
    | g.PatternConcatenate(_, _, _, _)
    | g.PatternBitString(_, _) -> Error(Unsupported("pattern"))
  }
}

fn clause_to_case(clause: g.Clause) -> Result(#(ast.Pat, ast.Expr), Error) {
  let g.Clause(patterns, guard, body) = clause
  case guard {
    Some(_) -> Error(Unsupported("case guard"))
    None ->
      case patterns {
        [[single]] ->
          map2(pattern(single), expression(body), fn(pat, body_expr) {
            #(pat, body_expr)
          })
        _ -> Error(Unsupported("case patterns"))
      }
  }
}

fn block_to_expression(
  statements: List(g.Statement),
  loc: Int,
) -> Result(ast.Expr, Error) {
  block_to_expression_loop(statements, [], loc)
}

fn block_to_expression_loop(
  statements: List(g.Statement),
  bindings: List(#(ast.Pat, ast.Expr)),
  loc: Int,
) -> Result(ast.Expr, Error) {
  case statements {
    [] -> Error(Unsupported("empty block"))

    [g.Expression(expr)] ->
      result.map(expression(expr), fn(body_expr) {
        case bindings {
          [] -> body_expr
          _ -> ast.Elet(list.reverse(bindings), body_expr, loc)
        }
      })

    [g.Expression(_), ..] -> Error(Unsupported("block expression position"))

    [g.Assignment(_span, kind, pat, _annotation, value), ..rest] ->
      case kind {
        g.Let ->
          map2(pattern(pat), expression(value), fn(pat_expr, value_expr) {
            block_to_expression_loop(
              rest,
              [#(pat_expr, value_expr), ..bindings],
              loc,
            )
          })
          |> result.flatten
        g.LetAssert(_) -> Error(Unsupported("let assert"))
      }

    [g.Assert(_, _, _), ..] -> Error(Unsupported("assert statement"))
    [g.Use(_, _, _), ..] -> Error(Unsupported("use statement"))
  }
}

fn field_arguments(
  fields: List(g.Field(g.Expression)),
) -> Result(List(ast.Expr), Error) {
  list.map(fields, fn(field) {
    case field {
      g.UnlabelledField(item) -> expression(item)
      g.LabelledField(_, _, _) -> Error(Unsupported("labelled arguments"))
      g.ShorthandField(_, _) -> Error(Unsupported("shorthand arguments"))
    }
  })
  |> result.all
}

fn field_patterns(
  fields: List(g.Field(g.Pattern)),
) -> Result(List(ast.Pat), Error) {
  list.map(fields, fn(field) {
    case field {
      g.UnlabelledField(item) -> pattern(item)
      g.LabelledField(_, _, _) -> Error(Unsupported("labelled constructor"))
      g.ShorthandField(_, _) -> Error(Unsupported("shorthand constructor"))
    }
  })
  |> result.all
}

fn fn_parameters(params: List(g.FnParameter)) -> Result(List(ast.Pat), Error) {
  list.map(params, fn(param) {
    let g.FnParameter(name, _type_) = param
    case name {
      g.Named(name) -> Ok(ast.Pvar(name, 0))
      g.Discarded(_) -> Ok(ast.Pany(0))
    }
  })
  |> result.all
}

fn definition_to_constant(
  def: g.Definition(g.Constant),
) -> Result(ast.Top, Error) {
  let g.Definition(_attributes, constant) = def
  let g.Constant(span, name, _pub, _annotation, value) = constant
  result.map(expression(value), fn(expr) {
    ast.Tdef(name, loc_from_span(span), expr, loc_from_span(span))
  })
}

fn definition_to_function(
  def: g.Definition(g.Function),
) -> Result(ast.Top, Error) {
  let g.Definition(_attributes, function) = def
  let g.Function(span, name, _pub, parameters, _return, body) = function

  case
    list.filter(parameters, fn(p) {
      let g.FunctionParameter(label, _name, _type_) = p
      label != None
    })
  {
    [] ->
      map2(
        function_parameters(parameters),
        block_to_expression(body, loc_from_span(span)),
        fn(params, body_expr) {
          let expr = ast.Elambda(params, body_expr, loc_from_span(span))
          ast.Tdef(name, loc_from_span(span), expr, loc_from_span(span))
        },
      )
    _ -> Error(Unsupported("labelled parameters"))
  }
}

fn definition_to_type_alias(
  def: g.Definition(g.TypeAlias),
) -> Result(ast.Top, Error) {
  let g.Definition(_attributes, alias) = def
  let g.TypeAlias(span, name, _pub, parameters, aliased) = alias

  result.map(type_(aliased), fn(type_expr) {
    let args = list.map(parameters, fn(param) { #(param, loc_from_span(span)) })
    ast.Ttypealias(
      name,
      loc_from_span(span),
      args,
      type_expr,
      loc_from_span(span),
    )
  })
}

fn definition_to_custom_type(
  def: g.Definition(g.CustomType),
) -> Result(ast.Top, Error) {
  let g.Definition(_attributes, custom) = def
  let g.CustomType(span, name, _pub, _opaque, parameters, variants) = custom

  result.map(custom_variants(variants, span), fn(constructors) {
    let args = list.map(parameters, fn(param) { #(param, loc_from_span(span)) })
    ast.Tdeftype(
      name,
      loc_from_span(span),
      args,
      constructors,
      loc_from_span(span),
    )
  })
}

fn custom_variants(
  variants: List(g.Variant),
  span: g.Span,
) -> Result(List(#(String, Int, List(types.Type), Int)), Error) {
  list.map(variants, fn(variant) {
    let g.Variant(name, fields, _attributes) = variant
    result.map(variant_fields(fields), fn(field_types) {
      #(name, loc_from_span(span), field_types, loc_from_span(span))
    })
  })
  |> result.all
}

fn variant_fields(
  fields: List(g.VariantField),
) -> Result(List(types.Type), Error) {
  list.map(fields, fn(field) {
    case field {
      g.LabelledVariantField(type_expr, _label) -> type_(type_expr)
      g.UnlabelledVariantField(type_expr) -> type_(type_expr)
    }
  })
  |> result.all
}

fn function_parameters(
  params: List(g.FunctionParameter),
) -> Result(List(ast.Pat), Error) {
  list.map(params, fn(param) {
    let g.FunctionParameter(_label, name, _type_) = param
    case name {
      g.Named(name) -> Ok(ast.Pvar(name, 0))
      g.Discarded(_) -> Ok(ast.Pany(0))
    }
  })
  |> result.all
}

pub fn type_(type_expr: g.Type) -> Result(types.Type, Error) {
  case type_expr {
    g.NamedType(span, name, module, parameters) ->
      case module {
        Some(_) -> Error(Unsupported("qualified type"))
        None ->
          result.map(result.all(list.map(parameters, type_)), fn(params) {
            list.fold(
              params,
              types.Tcon(name, loc_from_span(span)),
              fn(acc, param) { types.Tapp(acc, param, loc_from_span(span)) },
            )
          })
      }

    g.VariableType(span, name) -> Ok(types.Tvar(name, loc_from_span(span)))

    g.FunctionType(span, parameters, return_type) ->
      map2(
        result.all(list.map(parameters, type_)),
        type_(return_type),
        fn(args, result_type) {
          types.tfns(args, result_type, loc_from_span(span))
        },
      )

    g.TupleType(span, elements) ->
      case elements {
        [left, right] ->
          map2(type_(left), type_(right), fn(left_t, right_t) {
            types.Tapp(
              types.Tapp(
                types.Tcon(",", loc_from_span(span)),
                left_t,
                loc_from_span(span),
              ),
              right_t,
              loc_from_span(span),
            )
          })
        _ -> Error(Unsupported("tuple type arity"))
      }

    g.HoleType(_, _) -> Error(Unsupported("type hole"))
  }
}

fn loc_from_span(span: g.Span) -> Int {
  let g.Span(start, _end) = span
  start
}

fn map2(a: Result(x, e), b: Result(y, e), f: fn(x, y) -> z) -> Result(z, e) {
  result.try(a, fn(av) { result.map(b, fn(bv) { f(av, bv) }) })
}
