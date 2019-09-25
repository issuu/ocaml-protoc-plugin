let%expect_test _ =
  let module T = Recursive.Message in
  let t = T.{ m = Some { m = Some { m = None } }} in
  Test_lib.test_encode (module T) t;
  [%expect {| m {;  m {;  }; }; |}]
