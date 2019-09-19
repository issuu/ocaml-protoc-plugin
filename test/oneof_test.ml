open Base

let%expect_test _ =
  let module T = Oneof.Test in
  let t = T.{ y = 5; x = `J 7} in
  Test_lib.test_encode "oneof.proto" (module T) t;
  [%expect {|
    y: 5;j: 7; |}]
