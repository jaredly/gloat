import glance as g

pub type TypeError {
  TypeError(message: String, span: g.Span)
}

pub fn new(message: String, span: g.Span) -> TypeError {
  TypeError(message, span)
}

pub fn message(error: TypeError) -> String {
  let TypeError(message, _span) = error
  message
}

pub fn span(error: TypeError) -> g.Span {
  let TypeError(_message, span) = error
  span
}
