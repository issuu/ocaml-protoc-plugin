let%expect_test _ =
  let module T = Oneof.Test in
  let t = T.{ y = 5; x = `I 7} in
  Test_lib.test_encode ~dump:true (module T) t;
  [%expect {|
    Buffer: '08-05-50-07'
    y: 5; i: 7; |}]

let%expect_test _ =
  let module T = Oneof.Test2 in
  let t = T.{ x = `F3 "test"} in
  Test_lib.test_encode (module T) t;
  [%expect {|
    f3: "test"; |}]
