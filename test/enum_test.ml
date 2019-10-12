let%expect_test _ =
  let module T = Enum.Message in
  let t = T.{enum = Enum.Message.E.B} in
  Test_lib.test_encode (module T) t;
  [%expect {| enum: B |}]

let%expect_test _ =
  let module T = Enum.Outside in
  let t = T.{enum = Enum.E1.C} in
  Test_lib.test_encode (module T) t;
  [%expect {| enum: C |}]

let%expect_test _ =
  let module T = Enum.Aliasing in
  let t = T.{e = T.Enum.Z} in
  Test_lib.test_encode (module T) t;
  (* We do expect the enum to be deserialized as Y. *)
  [%expect {|
    e: Y

    Expect  :{ e = Z }
    Observed:{ e = Y } |}]

let%expect_test _ =
  let module T = Enum.Negative in
  let t = T.{e = T.Enum.A3} in
  Test_lib.test_encode (module T) t;
  [%expect {| e: A3 |}]
