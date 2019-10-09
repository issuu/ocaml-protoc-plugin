let service reader =
  let (s_deser, s_ser) =
    Protobuf.Service.make_service_functions Service.String_of_int.call
  in
  let req =
    s_deser reader
    |> (function Ok v -> v | Error _ -> failwith "Error")
  in
  let s =
    req.Service.Request.i
    |> string_of_int
  in
  Service.Response.{ s }
  |> s_ser

let call i =
  let (c_ser, c_deser) =
    Protobuf.Service.make_client_functions Service.String_of_int.call
  in
  let req = Service.Request.{ i } in
  req
  |> c_ser
  |> Protobuf.Writer.contents
  |> Protobuf.Reader.create
  |> service
  |> Protobuf.Writer.contents
  |> Protobuf.Reader.create
  |> c_deser
  |> (function Ok r -> r.Service.Response.s | Error _ -> failwith "Error")

let%expect_test _ =
  Caml.Printf.printf "%d -> \"%s\"\n" 0 (call 0);
  Caml.Printf.printf "%d -> \"%s\"\n" 5 (call 5);
  Caml.Printf.printf "%d -> \"%s\"\n" 50 (call 50);
  Caml.Printf.printf "%d -> \"%s\"\n" (-5) (call (-5));
  Caml.Printf.printf "%d -> \"%s\"\n" (-100) (call (-100));
  ();
  [%expect {|
    0 -> "0"
    5 -> "5"
    50 -> "50"
    -5 -> "-5"
    -100 -> "-100" |}]
