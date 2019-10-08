let%expect_test "Packed as string" =
  let module T = Packed.Packed in
  let module T' = Packed.String in
  let t = T.{i = [5; 6; 7; 8; 9]} in
  Test_lib.test_encode (module T) t;
  T.to_proto t
  |> Protobuf.Writer.contents
  |> Protobuf.Reader.create
  |> T'.from_proto
  |> (function
      | Ok t -> print_endline (T'.show t)
      | Error e -> Printf.printf "Failed to decode: %s\n" (Protobuf.Deserialize.show_error e)
    );
  [%expect {|
    i: 5
    i: 6
    i: 7
    i: 8
    i: 9
    { s = "\005\006\007\b\t" } |}]

let%expect_test "Packed as int" =
  let module T = Packed.Not_packed in
  let module T' = Packed.UInt in
  let t = T.{i = [5; 6; 7; 8; 9]} in
  Test_lib.test_encode (module T) t;
  T.to_proto t
  |> Protobuf.Writer.contents
  |> Protobuf.Reader.create
  |> T'.from_proto
  |> (function
      | Ok t -> print_endline (T'.show t)
      | Error e -> Printf.printf "Failed to decode: %s\n" (Protobuf.Deserialize.show_error e)
    );
  [%expect {|
    i: 5
    i: 6
    i: 7
    i: 8
    i: 9
    { i = 9 } |}]
