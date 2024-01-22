open Large
open Ocaml_protoc_plugin

let%expect_test "Test very large message type" =
  let large = Large.make ~x7:7 () in
  let writer = Large.to_proto large in
  let contents = Writer.contents writer in
  Printf.printf "Size of large message: %d\n" (String.length contents);
  let reader = Reader.create contents in
  let large' = Large.from_proto_exn reader in
  Printf.printf "Serialization works: %b\n" (large = large');
  Printf.printf "x7: %d = %d\n" large.x7 large'.x7;
  Printf.printf "x5: %d = %d\n" large.x5 large'.x5;
  ();
  [%expect {|
    Size of large message: 2
    Serialization works: true
    x7: 7 = 7
    x5: 0 = 0 |}]
