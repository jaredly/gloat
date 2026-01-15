import gleam/list
import gleam/result
import gleam/string

pub fn parse_int_literal(value: String) -> Result(Int, Nil) {
  let normalized = string.replace(value, "_", "")
  case normalized {
    "" -> Error(Nil)
    _ ->
      case string.starts_with(normalized, "-") {
        True ->
          result.map(parse_int_literal(string.drop_start(normalized, 1)), fn(v) {
            -v
          })
        False ->
          case
            string.starts_with(normalized, "0x")
            || string.starts_with(normalized, "0X")
          {
            True -> parse_base(16, string.drop_start(normalized, 2))
            False ->
              case
                string.starts_with(normalized, "0o")
                || string.starts_with(normalized, "0O")
              {
                True -> parse_base(8, string.drop_start(normalized, 2))
                False ->
                  case
                    string.starts_with(normalized, "0b")
                    || string.starts_with(normalized, "0B")
                  {
                    True -> parse_base(2, string.drop_start(normalized, 2))
                    False -> parse_base(10, normalized)
                  }
              }
          }
      }
  }
}

fn parse_base(base: Int, digits: String) -> Result(Int, Nil) {
  let graphemes = string.to_graphemes(digits)
  case graphemes {
    [] -> Error(Nil)
    _ ->
      list.fold(graphemes, Ok(0), fn(acc, ch) {
        case acc {
          Error(_) -> Error(Nil)
          Ok(value) ->
            case digit_value(ch) {
              Ok(digit) ->
                case digit < base {
                  True -> Ok(value * base + digit)
                  False -> Error(Nil)
                }
              Error(_) -> Error(Nil)
            }
        }
      })
  }
}

fn digit_value(char: String) -> Result(Int, Nil) {
  case char {
    "0" -> Ok(0)
    "1" -> Ok(1)
    "2" -> Ok(2)
    "3" -> Ok(3)
    "4" -> Ok(4)
    "5" -> Ok(5)
    "6" -> Ok(6)
    "7" -> Ok(7)
    "8" -> Ok(8)
    "9" -> Ok(9)
    "a" | "A" -> Ok(10)
    "b" | "B" -> Ok(11)
    "c" | "C" -> Ok(12)
    "d" | "D" -> Ok(13)
    "e" | "E" -> Ok(14)
    "f" | "F" -> Ok(15)
    _ -> Error(Nil)
  }
}
