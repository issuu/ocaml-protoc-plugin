open Proto3_optional

let%expect_test _ =
  let module T = Proto3_optional.Message in
  let t = T.make ~payload:5 () in
  Test_lib.test_encode ~protoc:false (module T) t;
  Printf.printf "%s" ([%show: T.t] t);
  [%expect {| (Some 5) |}]
