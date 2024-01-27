module Enum = Enum.Enum_test
let%expect_test _ =
  let module T = Enum.Message in
  let t = Enum.Message.E.B in
  Test_lib.test_encode (module T) t;
  [%expect {| enum: B |}]

let%expect_test _ =
  let module T = Enum.Outside in
  let t = Enum.E1.C in
  Test_lib.test_encode (module T) t;
  [%expect {| enum: C |}]

let%expect_test _ =
  let module T = Enum.Aliasing in
  let t = T.Enum.Z in
  (* Due to aliasing, we expect this to be deserialized as 'Y'. *)
  Test_lib.test_encode (module T) ~expect:T.Enum.Y t;
  [%expect {|
    e: Y |}]

let%expect_test _ =
  let module T = Enum.Negative in
  let t = T.Enum.A3 in
  Test_lib.test_encode (module T) t;
  [%expect {| e: A3 |}]
