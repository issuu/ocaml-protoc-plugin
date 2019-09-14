open Core

let%expect_test _ =
  let module T = Repeated.UInt64 in
  let t = T.{ i = [0; 1;2;3;4] } in
  Test_lib.test_encode "repeated.proto" (module T) t;
  [%expect{|
    i: 0
    i: 1
    i: 2
    i: 3
    i: 4 |}]

let%expect_test _ =
  let module T = Repeated.Double in
  let t = T.{ i = [0.; 1.;2.;3.;4.] } in
  Test_lib.test_encode "repeated.proto" (module T) t;
  [%expect{|
    i: 0
    i: 1
    i: 2
    i: 3
    i: 4 |}]

let%expect_test _ =
  let module T = Repeated.Float in
  let t = T.{ i = [0.; 1.;2.;3.;4.] } in
  Test_lib.test_encode "repeated.proto" (module T) t;
  [%expect{|
    i: 0
    i: 1
    i: 2
    i: 3
    i: 4 |}]

let%expect_test _ =
  let module T = Repeated.String in
  let t = T.{ i = ["0"; "1";"2";"3";"4"] } in
  Test_lib.test_encode "repeated.proto" (module T) t;
  [%expect{|
    i: "0"
    i: "1"
    i: "2"
    i: "3"
    i: "4" |}]
