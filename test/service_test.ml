open Service
module S = Service.P1.P2.P3
let service reader =
  let (s_deser, s_ser) =
    Ocaml_protoc_plugin.Service.make_service_functions S.String_of_int.call
  in
  let req =
    s_deser reader
    |> (function Ok v -> v | Error _ -> failwith "Error")
  in
  string_of_int req |> s_ser

let call i =
  let (c_ser, c_deser) =
    Ocaml_protoc_plugin.Service.make_client_functions S.String_of_int.call
  in
  let req = i in
  req
  |> c_ser
  |> Ocaml_protoc_plugin.Writer.contents
  |> Ocaml_protoc_plugin.Reader.create
  |> service
  |> Ocaml_protoc_plugin.Writer.contents
  |> Ocaml_protoc_plugin.Reader.create
  |> c_deser
  |> (function Ok r -> r | Error _ -> failwith "Error")

let%expect_test _ =
  Printf.printf "name: \"%s\"\n" S.String_of_int.Call.name;
  Printf.printf "package_name: \"%s\"\n" @@ Option.value ~default:"<none>" S.String_of_int.Call.package_name;
  Printf.printf "service_name: \"%s\"\n" S.String_of_int.Call.service_name;
  Printf.printf "method_name: \"%s\"\n" S.String_of_int.Call.method_name;
  [%expect {|
    name: "/service.p1.p2.p3.String_of_int/Call"
    package_name: "service.p1.p2.p3"
    service_name: "String_of_int"
    method_name: "Call" |}]

let%expect_test _ =
  Printf.printf "%d -> \"%s\"\n" 0 (call 0);
  Printf.printf "%d -> \"%s\"\n" 5 (call 5);
  Printf.printf "%d -> \"%s\"\n" 50 (call 50);
  Printf.printf "%d -> \"%s\"\n" (-5) (call (-5));
  Printf.printf "%d -> \"%s\"\n" (-100) (call (-100));
  ();
  [%expect {|
    0 -> "0"
    5 -> "5"
    50 -> "50"
    -5 -> "-5"
    -100 -> "-100" |}]
