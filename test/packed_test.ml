open Packed
let%expect_test "Packed" =
  let module T = Packed.Packed in
  let module T' = Packed.String in
  let t = [5; 6; 0; 7; 8; 9] in
  Test_lib.test_encode (module T) t;
  let contents =
    T.to_proto t
    |> Ocaml_protoc_plugin.Writer.contents
  in
  contents
  |> Ocaml_protoc_plugin.Reader.create
  |> T'.from_proto
  |> (function
      | Ok t -> Printf.printf "Data: %s. Size: %d\n" (T'.show t) (String.length contents)
      | Error e -> Printf.printf "Failed to decode: %s\n" (Ocaml_protoc_plugin.Result.show_error e)
    );
  [%expect {|
    i: 5
    i: 6
    i: 0
    i: 7
    i: 8
    i: 9
    Data: "\005\006\000\007\b\t". Size: 8 |}]

let%expect_test "Not packed" =
  let module T = Packed.Not_packed in
  let module T' = Packed.UInt in
  let t = [5; 6; 0; 7; 8; 9] in
  Test_lib.test_encode (module T) t;
  let contents =
    T.to_proto t
    |> Ocaml_protoc_plugin.Writer.contents
  in
  contents
  |> Ocaml_protoc_plugin.Reader.create
  |> T'.from_proto
  |> (function
      | Ok t -> Printf.printf "Last element: %s. Size: %d\n" (T'.show t) (String.length contents)

      | Error e -> Printf.printf "Failed to decode: %s\n" (Ocaml_protoc_plugin.Result.show_error e)
    );
  [%expect {|
    i: 5
    i: 6
    i: 0
    i: 7
    i: 8
    i: 9
    Last element: 9. Size: 12 |}]

(* Verify that empty lists are not serialized at all *)
let%expect_test "Empty lists are not transmitted" =
  Test_lib.test_encode (module Packed.Packed) [];
  Packed.Packed.to_proto []
  |> Ocaml_protoc_plugin.Writer.contents
  |> String.length
  |> Printf.eprintf "Size packed %d\n";

  Test_lib.test_encode (module Packed.Not_packed) [];
  Packed.Not_packed.to_proto []
  |> Ocaml_protoc_plugin.Writer.contents
  |> String.length
  |> Printf.eprintf "Size packed %d\n";
  ();
  [%expect {|
    Size packed 0
    Size packed 0 |}]
