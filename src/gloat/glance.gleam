import glance as g
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import gloat/ast
import gloat/types

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

    g.Float(span, value) ->
      case float.parse(value) {
        Ok(float_value) ->
          Ok(ast.Eprim(
            ast.Pfloat(float_value, loc_from_span(span)),
            loc_from_span(span),
          ))
        Error(_) -> Error(Unsupported("float literal"))
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
      result.map(result.all(list.map(elements, expression)), fn(exprs) {
        case exprs {
          [_, ..] -> Ok(ast.Etuple(exprs, loc_from_span(span)))
          _ -> Error(Unsupported("tuple arity"))
        }
      })
      |> result.flatten

    g.List(span, elements, tail) ->
      map2(
        result.all(list.map(elements, expression)),
        list_tail_expr(tail),
        fn(items, tail_expr) {
          ast.Elist(items, tail_expr, loc_from_span(span))
        },
      )

    g.BitString(span, segments) ->
      result.map(bit_string_segments_expr(segments), fn(segments) {
        ast.Ebitstring(segments, loc_from_span(span))
      })

    g.Call(span, function, arguments) ->
      map2(
        expression(function),
        field_arguments(arguments),
        fn(func_expr, args) { call_with_fields(func_expr, args, span) },
      )
      |> result.flatten

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
          g.Concatenate -> Ok("<>")
          g.Pipe -> Ok("|>")
          _ -> Error(Unsupported("binary operator " <> string.inspect(op)))
        }

        result.map(name, fn(n) {
          let loc = loc_from_span(span)
          ast.Eapp(ast.Evar(n, loc), [left_expr, right_expr], loc)
        })
      })
      |> result.flatten

    g.NegateInt(span, expr) ->
      result.map(expression(expr), fn(expr) {
        let loc = loc_from_span(span)
        ast.Eapp(ast.Evar("negate", loc), [expr], loc)
      })

    g.NegateBool(span, expr) ->
      result.map(expression(expr), fn(expr) {
        let loc = loc_from_span(span)
        ast.Eapp(ast.Evar("not", loc), [expr], loc)
      })

    g.TupleIndex(span, target, index) ->
      result.map(expression(target), fn(expr) {
        ast.EtupleIndex(expr, index, loc_from_span(span))
      })

    g.FnCapture(span, label, function, args_before, args_after) ->
      fn_capture_to_expr(span, label, function, args_before, args_after)

    g.Echo(span, expression, message) ->
      map2(option_expr(expression), option_expr(message), fn(expr, msg) {
        ast.Eecho(expr, msg, loc_from_span(span))
      })

    g.Panic(span, maybe_expr) ->
      result.map(
        case maybe_expr {
          option.Some(expr) -> result.map(expression(expr), option.Some)
          option.None -> Ok(option.None)
        },
        fn(maybe_expr) {
          let loc = loc_from_span(span)
          ast.Eapp(
            ast.Evar("fatal", loc),
            case maybe_expr {
              option.Some(expr) -> [expr]
              option.None -> []
            },
            loc,
          )
        },
      )

    g.Todo(span, _label) -> {
      let loc = loc_from_span(span)
      Ok(ast.Eapp(ast.Evar("fatal", loc), [ast.Estr("todo", [], loc)], loc))
    }

    g.Case(span, subjects, clauses) ->
      case subjects {
        [] -> Error(Unsupported("case subject count"))
        _ ->
          map2(
            result.all(list.map(subjects, expression)),
            result.map(
              result.all(
                list.map(clauses, fn(clause) {
                  clause_to_cases(clause, list.length(subjects), span)
                }),
              ),
              list.flatten,
            ),
            fn(subject_exprs, cases) {
              let target_expr = case subject_exprs {
                [subject] -> subject
                _ -> ast.Etuple(subject_exprs, loc_from_span(span))
              }
              ast.Ematch(target_expr, cases, loc_from_span(span))
            },
          )
      }

    g.RecordUpdate(span, module, constructor, record, fields) ->
      map2(
        expression(record),
        record_update_fields(fields),
        fn(record_expr, update_fields) {
          ast.ErecordUpdate(
            module,
            constructor,
            record_expr,
            update_fields,
            loc_from_span(span),
          )
        },
      )

    g.FieldAccess(span, container, label) ->
      result.map(expression(container), fn(expr) {
        ast.Efield(expr, label, loc_from_span(span))
      })
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
    g.PatternFloat(span, value) ->
      case float.parse(value) {
        Ok(float_value) ->
          Ok(ast.Pprim(
            ast.Pfloat(float_value, loc_from_span(span)),
            loc_from_span(span),
          ))
        Error(_) -> Error(Unsupported("float pattern"))
      }
    g.PatternString(span, value) -> Ok(ast.Pstr(value, loc_from_span(span)))

    g.PatternTuple(span, elements) ->
      result.map(result.all(list.map(elements, pattern)), fn(pats) {
        case pats {
          [_, ..] -> Ok(ast.Ptuple(pats, loc_from_span(span)))
          _ -> Error(Unsupported("tuple pattern arity"))
        }
      })
      |> result.flatten

    g.PatternList(span, elements, tail) ->
      map2(
        result.all(list.map(elements, pattern)),
        pattern_list_tail(tail),
        fn(pats, tail_pat) { ast.Plist(pats, tail_pat, loc_from_span(span)) },
      )

    g.PatternAssignment(span, pat, name) ->
      result.map(pattern(pat), fn(pat) {
        ast.Pas(name, pat, loc_from_span(span))
      })

    g.PatternConcatenate(span, prefix, prefix_name, rest_name) ->
      Ok(ast.Pconcat(
        prefix,
        assignment_name_to_option(prefix_name),
        assignment_name_to_option(option.Some(rest_name)),
        loc_from_span(span),
      ))

    g.PatternBitString(span, segments) ->
      result.map(bit_string_segments_pat(segments), fn(segments) {
        ast.Pbitstring(segments, loc_from_span(span))
      })

    g.PatternVariant(span, module, constructor, arguments, with_spread) ->
      case module, with_spread {
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
  }
}

fn clause_to_cases(
  clause: g.Clause,
  subject_count: Int,
  span: g.Span,
) -> Result(List(#(ast.Pat, option.Option(ast.Expr), ast.Expr)), Error) {
  let g.Clause(patterns, guard, body) = clause
  map2(
    result.all(
      list.map(patterns, fn(patterns) {
        patterns_to_pat(patterns, subject_count, span)
      }),
    ),
    map2(option_expr(guard), expression(body), fn(guard, body_expr) {
      #(guard, body_expr)
    }),
    fn(pats, guard_body) {
      let #(guard, body_expr) = guard_body
      list.map(pats, fn(pat) { #(pat, guard, body_expr) })
    },
  )
}

fn patterns_to_pat(
  patterns: List(g.Pattern),
  subject_count: Int,
  span: g.Span,
) -> Result(ast.Pat, Error) {
  case subject_count {
    1 ->
      case patterns {
        [single] -> pattern(single)
        _ -> Error(Unsupported("case patterns"))
      }
    _ ->
      case patterns {
        [] -> Error(Unsupported("case patterns"))
        _ ->
          case list.length(patterns) == subject_count {
            True ->
              result.map(result.all(list.map(patterns, pattern)), fn(pats) {
                ast.Ptuple(pats, loc_from_span(span))
              })
            False -> Error(Unsupported("case patterns"))
          }
      }
  }
}

fn pattern_list_tail(
  tail: option.Option(g.Pattern),
) -> Result(option.Option(ast.Pat), Error) {
  case tail {
    option.None -> Ok(option.None)
    option.Some(pat) -> result.map(pattern(pat), fn(pat) { option.Some(pat) })
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

fn list_tail_expr(
  tail: option.Option(g.Expression),
) -> Result(option.Option(ast.Expr), Error) {
  case tail {
    None -> Ok(None)
    Some(expr) -> result.map(expression(expr), fn(expr) { Some(expr) })
  }
}

fn option_expr(
  expr: option.Option(g.Expression),
) -> Result(option.Option(ast.Expr), Error) {
  case expr {
    option.None -> Ok(option.None)
    option.Some(expr) ->
      result.map(expression(expr), fn(expr) { option.Some(expr) })
  }
}

fn assignment_name_to_option(
  name: option.Option(g.AssignmentName),
) -> option.Option(String) {
  case name {
    option.None -> option.None
    option.Some(g.Named(name)) -> option.Some(name)
    option.Some(g.Discarded(_)) -> option.None
  }
}

fn bit_string_segments_expr(
  segments: List(#(g.Expression, List(g.BitStringSegmentOption(g.Expression)))),
) -> Result(
  List(#(ast.Expr, List(ast.BitStringSegmentOption(ast.Expr)))),
  Error,
) {
  result.all(
    list.map(segments, fn(segment) {
      let #(expr, options) = segment
      map2(
        expression(expr),
        bit_string_options_expr(options),
        fn(expr, options) { #(expr, options) },
      )
    }),
  )
}

fn bit_string_segments_pat(
  segments: List(#(g.Pattern, List(g.BitStringSegmentOption(g.Pattern)))),
) -> Result(List(#(ast.Pat, List(ast.BitStringSegmentOption(ast.Pat)))), Error) {
  result.all(
    list.map(segments, fn(segment) {
      let #(pat, options) = segment
      map2(pattern(pat), bit_string_options_pat(options), fn(pat, options) {
        #(pat, options)
      })
    }),
  )
}

fn bit_string_options_expr(
  options: List(g.BitStringSegmentOption(g.Expression)),
) -> Result(List(ast.BitStringSegmentOption(ast.Expr)), Error) {
  result.all(
    list.map(options, fn(opt) {
      case opt {
        g.BytesOption -> Ok(ast.BytesOption)
        g.IntOption -> Ok(ast.IntOption)
        g.FloatOption -> Ok(ast.FloatOption)
        g.BitsOption -> Ok(ast.BitsOption)
        g.Utf8Option -> Ok(ast.Utf8Option)
        g.Utf16Option -> Ok(ast.Utf16Option)
        g.Utf32Option -> Ok(ast.Utf32Option)
        g.Utf8CodepointOption -> Ok(ast.Utf8CodepointOption)
        g.Utf16CodepointOption -> Ok(ast.Utf16CodepointOption)
        g.Utf32CodepointOption -> Ok(ast.Utf32CodepointOption)
        g.SignedOption -> Ok(ast.SignedOption)
        g.UnsignedOption -> Ok(ast.UnsignedOption)
        g.BigOption -> Ok(ast.BigOption)
        g.LittleOption -> Ok(ast.LittleOption)
        g.NativeOption -> Ok(ast.NativeOption)
        g.SizeValueOption(expr) ->
          result.map(expression(expr), fn(expr) { ast.SizeValueOption(expr) })
        g.SizeOption(size) -> Ok(ast.SizeOption(size))
        g.UnitOption(unit) -> Ok(ast.UnitOption(unit))
      }
    }),
  )
}

fn bit_string_options_pat(
  options: List(g.BitStringSegmentOption(g.Pattern)),
) -> Result(List(ast.BitStringSegmentOption(ast.Pat)), Error) {
  result.all(
    list.map(options, fn(opt) {
      case opt {
        g.BytesOption -> Ok(ast.BytesOption)
        g.IntOption -> Ok(ast.IntOption)
        g.FloatOption -> Ok(ast.FloatOption)
        g.BitsOption -> Ok(ast.BitsOption)
        g.Utf8Option -> Ok(ast.Utf8Option)
        g.Utf16Option -> Ok(ast.Utf16Option)
        g.Utf32Option -> Ok(ast.Utf32Option)
        g.Utf8CodepointOption -> Ok(ast.Utf8CodepointOption)
        g.Utf16CodepointOption -> Ok(ast.Utf16CodepointOption)
        g.Utf32CodepointOption -> Ok(ast.Utf32CodepointOption)
        g.SignedOption -> Ok(ast.SignedOption)
        g.UnsignedOption -> Ok(ast.UnsignedOption)
        g.BigOption -> Ok(ast.BigOption)
        g.LittleOption -> Ok(ast.LittleOption)
        g.NativeOption -> Ok(ast.NativeOption)
        g.SizeValueOption(pat) ->
          result.map(pattern(pat), fn(pat) { ast.SizeValueOption(pat) })
        g.SizeOption(size) -> Ok(ast.SizeOption(size))
        g.UnitOption(unit) -> Ok(ast.UnitOption(unit))
      }
    }),
  )
}

fn fn_capture_to_expr(
  span: g.Span,
  label: option.Option(String),
  function: g.Expression,
  args_before: List(g.Field(g.Expression)),
  args_after: List(g.Field(g.Expression)),
) -> Result(ast.Expr, Error) {
  case label {
    option.Some(_) -> Error(Unsupported("labelled capture"))
    option.None ->
      map2(
        expression(function),
        map2(
          field_arguments(args_before),
          field_arguments(args_after),
          fn(before, after) { #(before, after) },
        ),
        fn(func_expr, args) {
          let #(before, after) = args
          case
            list.any(before, fn(arg) {
              let #(label, _expr) = arg
              label != option.None
            })
            || list.any(after, fn(arg) {
              let #(label, _expr) = arg
              label != option.None
            })
          {
            True -> Error(Unsupported("labelled capture arguments"))
            False -> {
              let before_args =
                list.map(before, fn(arg) {
                  let #(_label, expr) = arg
                  expr
                })
              let after_args =
                list.map(after, fn(arg) {
                  let #(_label, expr) = arg
                  expr
                })
              let loc = loc_from_span(span)
              let capture_name = "capture"
              let hole = ast.Evar(capture_name, loc)
              let applied =
                ast.Eapp(
                  func_expr,
                  list.append(before_args, [hole, ..after_args]),
                  loc,
                )
              Ok(ast.Elambda([ast.Pvar(capture_name, loc)], applied, loc))
            }
          }
        },
      )
      |> result.flatten
  }
}

fn field_arguments(
  fields: List(g.Field(g.Expression)),
) -> Result(List(#(option.Option(String), ast.Expr)), Error) {
  list.map(fields, fn(field) {
    case field {
      g.UnlabelledField(item) ->
        result.map(expression(item), fn(expr) { #(option.None, expr) })
      g.LabelledField(label, _loc, item) ->
        result.map(expression(item), fn(expr) { #(option.Some(label), expr) })
      g.ShorthandField(label, loc) ->
        Ok(#(option.Some(label), ast.Evar(label, loc_from_span(loc))))
    }
  })
  |> result.all
}

fn call_with_fields(
  func_expr: ast.Expr,
  args: List(#(option.Option(String), ast.Expr)),
  span: g.Span,
) -> Result(ast.Expr, Error) {
  case
    list.all(args, fn(arg) {
      let #(label, _expr) = arg
      label == option.None
    })
  {
    True ->
      Ok(ast.Eapp(
        func_expr,
        list.map(args, fn(arg) {
          let #(_label, expr) = arg
          expr
        }),
        loc_from_span(span),
      ))
    False ->
      case func_expr {
        ast.Evar(name, _) ->
          Ok(ast.Erecord(option.None, name, args, loc_from_span(span)))
        ast.Efield(ast.Evar(module, _), name, _) ->
          Ok(ast.Erecord(option.Some(module), name, args, loc_from_span(span)))
        _ -> Error(Unsupported("labelled arguments"))
      }
  }
}

fn record_update_fields(
  fields: List(g.RecordUpdateField(g.Expression)),
) -> Result(List(#(String, ast.Expr)), Error) {
  result.all(
    list.map(fields, fn(field) {
      let g.RecordUpdateField(label, item) = field
      case item {
        option.Some(expr) ->
          result.map(expression(expr), fn(expr) { #(label, expr) })
        option.None -> Ok(#(label, ast.Evar(label, 0)))
      }
    }),
  )
}

fn field_patterns(
  fields: List(g.Field(g.Pattern)),
) -> Result(List(#(option.Option(String), ast.Pat)), Error) {
  list.map(fields, fn(field) {
    case field {
      g.UnlabelledField(item) ->
        result.map(pattern(item), fn(pat) { #(option.None, pat) })
      g.LabelledField(label, _loc, item) ->
        result.map(pattern(item), fn(pat) { #(option.Some(label), pat) })
      g.ShorthandField(label, loc) ->
        Ok(#(option.Some(label), ast.Pvar(label, loc_from_span(loc))))
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
      label != option.None
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
) -> Result(
  List(#(String, Int, List(#(option.Option(String), types.Type)), Int)),
  Error,
) {
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
        option.Some(_) -> Error(Unsupported("qualified type"))
        option.None -> {
          let resolved = case name {
            "BitArray" -> "BitString"
            _ -> name
          }
          result.map(result.all(list.map(parameters, type_)), fn(params) {
            list.fold(
              params,
              types.Tcon(resolved, loc_from_span(span)),
              fn(acc, param) { types.Tapp(acc, param, loc_from_span(span)) },
            )
          })
        }
      }

    g.VariableType(span, name) -> Ok(types.Tvar(name, loc_from_span(span)))

    g.FunctionType(span, parameters, return_type) ->
      map2(
        result.all(list.map(parameters, type_)),
        type_(return_type),
        fn(args, result_type) {
          types.Tfn(args, result_type, loc_from_span(span))
        },
      )

    g.TupleType(span, elements) ->
      result.map(result.all(list.map(elements, type_)), fn(types_) {
        case types_ {
          [_, ..] -> Ok(types.Ttuple(types_, loc_from_span(span)))
          _ -> Error(Unsupported("tuple type arity"))
        }
      })
      |> result.flatten

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
