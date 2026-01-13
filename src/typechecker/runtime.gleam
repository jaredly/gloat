import gleam/string

pub fn fatal(message: String) -> a {
  panic as message
}

pub fn jsonify(value: a) -> String {
  string.inspect(value)
}

pub fn value_to_string(value: a) -> String {
  string.inspect(value)
}

pub fn eval(_source: String) -> a {
  panic as "eval is not implemented"
}

pub fn eval_with(_ctx: a, _source: String) -> b {
  panic as "eval_with is not implemented"
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

pub fn replace_all(
  value: String,
  pattern: String,
  replacement: String,
) -> String {
  string.replace(value, pattern, replacement)
}
