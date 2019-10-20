open Oneof2
let%expect_test _ =
  let module T = Oneof.Test in
  let t = T.{ y = 5; x = `I 7} in
  Test_lib.test_encode ~dump:true (module T) t;
  [%expect {|
    Buffer: '08-05-50-07'
    y: 5
    i: 7 |}]

let%expect_test _ =
  let module T = Oneof.Test2 in
  let t = `F3 "test" in
  Test_lib.test_encode (module T) t;
  [%expect {|
    f3: "test" |}]

let%expect_test "Multiple oneofs" =
  let module T = Oneof.Test3 in
  let t = T.{ x = `X1 3; y = `Y2 5; z = `Z1 7 } in
  Test_lib.test_encode (module T) t;
  [%expect {|
    x1: 3
    y2: 5
    z1: 7 |}]

let%expect_test "Default values in oneof" =
  let module T = Oneof.Test3 in
  let t = T.{ x = `X1 0; y = `Y2 0; z = `Z2 0 } in
  Test_lib.test_encode (module T) t;
  [%expect {|
    x1: 0
    y2: 0
    z2: 0 |}]

let%expect_test "Single field oneof" =
  let module T = Oneof.Test4 in
  let t = 5 in
  Test_lib.test_encode (module T) t;
  [%expect {|
    i: 5 |}]

let%expect_test "Single field oneof" =
  let module T = Oneof.Test5 in
  let t = () in
  Test_lib.test_encode (module T) t;
  [%expect {|
    e {
    } |}]
