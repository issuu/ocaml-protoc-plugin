open Core

let%expect_test _ =
  let module T = Repeated.UInt64 in
  let t = T.{ i = [0; 1;2;3;4] } in
  Test_lib.test_encode "repeated.proto" (module T) t;
  [%expect{|
    Size: 7
    i: 0;i: 1;i: 2;i: 3;i: 4; |}]

let%expect_test _ =
  let module T = Repeated.Double in
  let t = T.{ i = [0.; 1.;2.;3.;4.] } in
  Test_lib.test_encode "repeated.proto" (module T) t;
  [%expect{|
    Size: 42
    i: 0;i: 1;i: 2;i: 3;i: 4; |}]

let%expect_test _ =
  let module T = Repeated.Float in
  let t = T.{ i = [0.; 1.;2.;3.;4.] } in
  Test_lib.test_encode "repeated.proto" (module T) t;
  [%expect{|
    Size: 22
    i: 0;i: 1;i: 2;i: 3;i: 4; |}]

let%expect_test _ =
  let module T = Repeated.String in
  let t = T.{ i = ["0"; "1";"2";"3";"4"] } in
  Test_lib.test_encode "repeated.proto" (module T) t;
  [%expect{|
    Size: 15
    i: "0";i: "1";i: "2";i: "3";i: "4"; |}]

let%expect_test _ =
  let module T = Repeated.Enum in
  let t = T.{ e = T.E.[A; B; C; A; C] } in
  Test_lib.test_encode "repeated.proto" (module T) t;
  [%expect{|
    Size: 10
    e: A;e: B;e: C;e: A;e: C; |}]

let%expect_test _ =
  let module T = Repeated.Message in
  let m i = T.M.{ i } in
  let t = T.{ ms = [ m 0; m 1; m 2; m 1; m 0; m 5] } in
  Test_lib.test_encode "repeated.proto" (module T) t;
  [%expect{|
    Size: 20
    ms {;};ms {;  i: 1;};ms {;  i: 2;};ms {;  i: 1;};ms {;};ms {;  i: 5;}; |}]
