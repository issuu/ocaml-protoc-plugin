let%expect_test _ =
  let module T = Enum.Message in
  let t = T.{enum = Enum.Message.E.B} in
  Test_lib.test_encode (module T) t;
  [%expect {| enum: B |}]

let%expect_test _ =
  let module T = Enum.Outside in
  let t = T.{enum = Enum.E1.C} in
  Test_lib.test_encode (module T) t;
  [%expect {| enum: C |}]
