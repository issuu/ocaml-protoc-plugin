open Proto3_optional

let%expect_test _ =
  let module T = Proto3_optional.Message in
  let t = T.make ~payload:5 () in
  Test_lib.test_encode ~protoc_args:["--experimental_allow_proto3_optional"] (module T) t;
  [%expect {| (Some 5) |}]

let%expect_test _ =
  let module T = Proto3_optional.Message2 in
  let t = T.make ~payload:5 ~payload3:7 () in
  Test_lib.test_encode ~protoc_args:["--experimental_allow_proto3_optional"] (module T) t;
  [%expect {| (Some 5) |}]
