import ported_helpers as helpers

pub fn let__test() {
  assert Ok("Int") == helpers.assert_infer("let x = 1 2")
}

pub fn let_1_test() {
  assert Ok("Int") == helpers.assert_infer("let x = 1 x")
}

pub fn let_2_test() {
  assert Ok("Float") == helpers.assert_infer("let x = 2.0 x")
}

pub fn let_3_test() {
  assert Ok("Int") == helpers.assert_infer("let x = 2 let y = x y")
}

pub fn let_4_test() {
  assert Ok("#(Int, Float)")
    == helpers.assert_infer("let #(#(_, _) as x, _) = #(#(0, 1.0), []) x")
}

pub fn let_5_test() {
  assert Ok("String") == helpers.assert_infer("let x: String = \"\" x")
}

pub fn let_6_test() {
  assert Ok("#(Int, Int)")
    == helpers.assert_infer("let x: #(Int, Int) = #(5, 5) x")
}

pub fn let_7_test() {
  assert Ok("#(Int, Float)")
    == helpers.assert_infer("let x: #(Int, Float) = #(5, 5.0) x")
}

pub fn let_8_test() {
  assert Ok("List(Int)")
    == helpers.assert_infer("let assert [1, 2, ..x]: List(Int) = [1,2,3] x")
}

pub fn let_9_test() {
  assert Ok("List(Int)")
    == helpers.assert_infer(
      "let assert #(5, [..x]): #(Int, List(Int)) = #(5, [1,2,3]) x",
    )
}

pub fn let_10_test() {
  assert Ok("List(Int)")
    == helpers.assert_infer(
      "let assert #(5.0, [..x]): #(Float, List(Int)) = #(5.0, [1,2,3]) x",
    )
}

pub fn let_11_test() {
  assert Ok("List(a)") == helpers.assert_infer("let x: List(_) = [] x")
}

pub fn let_12_test() {
  assert Ok("List(Int)") == helpers.assert_infer("let x: List(_) = [1] x")
}

pub fn let_13_test() {
  assert Ok("Int") == helpers.assert_infer("let assert [a] = [1] a")
}

pub fn let_14_test() {
  assert Ok("Int") == helpers.assert_infer("let assert [a, 2] = [1] a")
}

pub fn let_15_test() {
  assert Ok("Int") == helpers.assert_infer("let assert [a, .. b] = [1] a")
}

pub fn let_16_test() {
  assert Ok("Int") == helpers.assert_infer("let assert [a, .. _] = [1] a")
}

pub fn let_17_test() {
  assert Ok("fn(List(a)) -> a")
    == helpers.assert_infer("fn(x) { let assert [a] = x a }")
}

pub fn let_18_test() {
  assert Ok("fn(List(Int)) -> Int")
    == helpers.assert_infer("fn(x) { let assert [a] = x a + 1 }")
}

pub fn let_19_test() {
  assert Ok("Float") == helpers.assert_infer("let _x = 1 2.0")
}

pub fn let_20_test() {
  assert Ok("Float") == helpers.assert_infer("let _ = 1 2.0")
}

pub fn let_21_test() {
  assert Ok("Int") == helpers.assert_infer("let #(tag, x) = #(1.0, 1) x")
}

pub fn let_22_test() {
  assert Ok("fn(#(a, b)) -> a")
    == helpers.assert_infer("fn(x) { let #(a, b) = x a }")
}

pub fn let_23_test() {
  assert Ok("Int") == helpers.assert_infer("let assert [] = [] 1")
}

pub fn let_24_test() {
  assert Ok("Result(Int, a)")
    == helpers.assert_infer("let assert Ok(..) = Ok(10)")
}

pub fn let_25_test() {
  assert Ok("String")
    == helpers.assert_infer("let assert \"hello\" as a = \"hello\" a")
}

pub fn no_scoped_var_collision_test() {
  assert Ok("Int") == helpers.assert_infer("let x = 1 { let x = 1.0 } x")
}
