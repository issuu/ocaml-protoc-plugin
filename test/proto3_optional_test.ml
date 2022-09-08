open Proto3_optional

let%expect_test _ =
  let module T = Proto3_optional.Message in
  let t = T.make ~payload:5 () in
  Test_lib.test_encode (module T) t;
  [%expect {| payload: 5 |}]
