let%expect_test _ =
  let module T = Proto2.Message in
  let t = T.{enum = Some E.B; i = 0; j = 5; required = Required.{ a = Some 7 }; k = Some 5 } in
  Test_lib.test_encode (module T) t;
  [%expect {|
    enum: B
    i: 0
    j: 5
    required {
      a: 7
    }
    k: 5 |}]

let%expect_test "Default read default values" =
  let module T = Proto2.A in
  let () = match T.from_proto (Protobuf.Reader.create "") with
    | Ok t -> print_endline (T.show t)
    | Error e  -> Printf.printf "Decode failure: %s\n" (Protobuf.Result.show_error e)
  in ();
  [%expect {| { i = 4 } |}]

let%expect_test "Required fields must be in the message" =
  let module T = Proto2.Message1 in
  let () = match T.from_proto (Protobuf.Reader.create "") with
    | Ok t -> print_endline (T.show t)
    | Error e  -> Printf.printf "Decode failure: %s\n" (Protobuf.Result.show_error e)
  in ();
  [%expect {| Decode failure: `Required_field_missing |}]

let%expect_test "Only tramitting the required field" =
  let module T = Proto2.Message1_ in
  let writer = T.to_proto T.{ req = 0; } in
  let module T = Proto2.Message1 in
  let () = match T.from_proto (Protobuf.Writer.contents writer |> Protobuf.Reader.create) with
    | Ok t -> print_endline (T.show t)
    | Error e  -> Printf.printf "Decode failure: %s\n" (Protobuf.Result.show_error e)
  in ();
  [%expect {|
    { opt = None; req = 0; s = "default string"; u = 27; b = "default bytes";
      c = 27; f = 27.; e = B } |}]
