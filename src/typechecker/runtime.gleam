import gleam/string

pub fn fatal(message: String) -> a {
  panic(message)
}

pub fn jsonify(_value: a) -> String {
  "<json>"
}

pub fn value_to_string(_value: a) -> String {
  "<value>"
}

pub fn eval(_source: String) -> a {
  panic("eval is not implemented")
}

pub fn eval_with(_ctx: a, _source: String) -> b {
  panic("eval_with is not implemented")
}

pub fn error_to_string(f: fn(a) -> String, value: a) -> String {
  f(value)
}

pub fn unescape_string(value: String) -> String {
  value
}

pub fn sanitize(value: String) -> String {
  value
}

pub fn replace_all(value: String, pattern: String, replacement: String) -> String {
  string.replace(value, pattern, replacement)
}
