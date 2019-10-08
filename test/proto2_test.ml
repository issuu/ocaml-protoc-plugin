let%expect_test _ =
  let module T = Proto2.Message in
  let t = T.{enum = T.E.B; i = 0;} in
  Test_lib.test_encode (module T) t;
  [%expect {|
    enum: B
    i: 0 |}]

let%expect_test "Default read default values" =
  let module T = Proto2.A in
  let () = match T.from_proto (Protobuf.Reader.create "") with
    | Ok t -> print_endline (T.show t)
    | Error _ -> print_endline "Failed to decode"
  in ();
  [%expect {| { i = 4 } |}]
