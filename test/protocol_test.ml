open Protocol
let%test "Last value kept"  =
  let messages = List.init 8 (fun i -> Protocol.Message.{ i }) in
  let oneof_messages = [] in
  let t = Protocol.Old.{ messages; oneof_i = "Oneof_test"; oneof_j = 13; oneof_messages } in

  let expect = Protocol.New.{ message = Some Protocol.Message.{ i = 7 }; oneof = `Oneof_j 13 } in
  let writer = Protocol.Old.to_proto t in
  let reader = Ocaml_protoc_plugin.Writer.contents writer |> Ocaml_protoc_plugin.Reader.create in

  match Protocol.New.from_proto reader with
  | Ok t ->
    Protocol.New.equal t expect
  | Error _ -> false

let%test "Last value kept - 2"  =
  let messages = List.init 8 (fun i -> Protocol.Message.{ i }) in
  let oneof_messages = [] in
  let t = Protocol.Old.{ messages; oneof_i = "Oneof_test"; oneof_j = 13; oneof_messages } in

  let expect = Protocol.New.{ message = Some Protocol.Message.{ i = 7 }; oneof = `Oneof_j 13 } in
  let writer = Protocol.Old.to_proto t in
  let reader = Ocaml_protoc_plugin.Writer.contents writer ^ Ocaml_protoc_plugin.Writer.contents writer |> Ocaml_protoc_plugin.Reader.create in

  match Protocol.New.from_proto reader with
  | Ok t ->
    Protocol.New.equal t expect
  | Error _ -> false

let%test "Repeated fields kept as it should"  =
  let is1 = List.init 8 (fun i -> i + 6) in
  let is2 = List.init 8 (fun i -> i + 17) in
  let t1 = Protocol.List.{ is = is1 } in
  let t2 = Protocol.List.{ is = is2 } in
  let expect = Protocol.List.{ is = is1 @ is2 } in
  let writer1 = Protocol.List.to_proto t1 in
  let writer2 = Protocol.List.to_proto t2 in
  let reader = Ocaml_protoc_plugin.Writer.contents writer1 ^ Ocaml_protoc_plugin.Writer.contents writer2 |> Ocaml_protoc_plugin.Reader.create in
  match Protocol.List.from_proto reader with
  | Ok t ->
    Protocol.List.equal t expect
  | Error _ ->
    false
