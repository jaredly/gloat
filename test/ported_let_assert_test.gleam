import ported_helpers as helpers

pub fn empty_list_test() {
  assert Ok("Int") == helpers.assert_infer("let assert [] = [] 1")
}

pub fn list_one_test() {
  assert Ok("Int") == helpers.assert_infer("let assert [a] = [1] a")
}

pub fn list_two_test() {
  assert Ok("Int") == helpers.assert_infer("let assert [a, 2] = [1] a")
}

pub fn list_spread_test() {
  assert Ok("Int") == helpers.assert_infer("let assert [a, ..] = [1] a")
}

pub fn list_spread_discard_test() {
  assert Ok("Int") == helpers.assert_infer("let assert [a, .._] = [1] a")
}

pub fn in_fn_test() {
  assert Ok("fn(List(a)) -> a")
    == helpers.assert_infer("fn(x) { let assert [a] = x a }")
}

pub fn in_fn_list_int_test() {
  assert Ok("fn(List(Int)) -> Int")
    == helpers.assert_infer("fn(x) { let assert [a] = x a + 1 }")
}

pub fn discard_named_test() {
  assert Ok("Float") == helpers.assert_infer("let assert _x = 1 2.0")
}

pub fn discard_test() {
  assert Ok("Float") == helpers.assert_infer("let assert _ = 1 2.0")
}

pub fn tuple_test() {
  assert Ok("Int") == helpers.assert_infer("let assert #(tag, x) = #(1.0, 1) x")
}

pub fn tuple_in_fn_test() {
  assert Ok("fn(#(a, b)) -> a")
    == helpers.assert_infer("fn(x) { let assert #(a, b) = x a }")
}

pub fn annotation_test() {
  assert Ok("Int") == helpers.assert_infer("let assert 5: Int = 5 5")
}

pub fn new_syntax_test() {
  assert Ok("Result(a, Int)")
    == helpers.assert_infer("let assert Ok(x) = Error(1)")
}

pub fn expression_test() {
  assert Ok("Int") == helpers.assert_infer("let assert x = 1")
}

pub fn expression1_test() {
  assert Ok("Int")
    == helpers.assert_infer("let assert x = { let assert x = 1 }")
}

pub fn expression2_test() {
  assert Ok("Float")
    == helpers.assert_infer("let assert x = { let assert x = 1. }")
}

pub fn expression3_test() {
  assert Ok("Int") == helpers.assert_infer("let assert 1 = 1")
}

pub fn message_test() {
  assert Ok("Int")
    == helpers.assert_infer(
      "\nlet assert Ok(inner) = Ok(10) as \"This clearly never fails\"\ninner\n",
    )
}
