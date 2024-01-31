open Protocol
let%expect_test "Last value kept" =
  let messages = List.init 8 (fun i -> i) in
  let oneof_messages = [] in
  let t = Protocol.Old.{ messages; oneof_i = "Oneof_test"; oneof_j = 13; oneof_messages } in

  let writer = Protocol.Old.to_proto t in
  let reader = Ocaml_protoc_plugin.Writer.contents writer |> Ocaml_protoc_plugin.Reader.create in
  Printf.printf "%s\n" (Protocol.New.from_proto_exn reader |> Protocol.New.show);
  [%expect {| { message = (Some 7); oneof = `Oneof_j (13) } |}]

let%expect_test "Last value kept - 2"  =
  let messages = List.init 8 (fun i -> i) in
  let oneof_messages = [] in
  let t = Protocol.Old.{ messages; oneof_i = "Oneof_test"; oneof_j = 13; oneof_messages } in

  let writer = Protocol.Old.to_proto t in
  let reader = Ocaml_protoc_plugin.Writer.contents writer ^ Ocaml_protoc_plugin.Writer.contents writer |> Ocaml_protoc_plugin.Reader.create in
  Printf.printf "%s" (Protocol.New.from_proto_exn reader |> Protocol.New.show);
  [%expect {| { message = (Some 7); oneof = `Oneof_j (13) } |}]

let%expect_test "Repeated fields kept as it should"  =
  let is1 = List.init 8 (fun i -> i + 6) in
  let is2 = List.init 8 (fun i -> i + 17) in
  let t1 = is1 in
  let t2 = is2 in
  let writer1 = Protocol.List.to_proto t1 in
  let writer2 = Protocol.List.to_proto t2 in
  let reader = Ocaml_protoc_plugin.Writer.contents writer1 ^ Ocaml_protoc_plugin.Writer.contents writer2 |> Ocaml_protoc_plugin.Reader.create in
  Printf.printf "%s" (Protocol.List.from_proto_exn reader |> Protocol.List.show);
  [%expect {| [6; 7; 8; 9; 10; 11; 12; 13; 17; 18; 19; 20; 21; 22; 23; 24] |}]
