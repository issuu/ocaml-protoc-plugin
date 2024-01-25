open Extensions

let%expect_test _ =
  let foo = Extensions.Foo.{ bar = Some 5; extensions' = Ocaml_protoc_plugin.Extensions.default } in
  let foo = Extensions.Baz'.set foo (Some 7) in
  let baz = Extensions.Baz'.get foo in
  print_endline ([%show: Extensions.Baz.t Ocaml_protoc_plugin.Result.t] baz);
  let () = match baz = Ok (Some 7) with
    | false -> print_endline "Failed. Not equal"
    | true -> ()
  in
  ();
  [%expect {| Ok (Some 7) |}]

let%expect_test _ =
  let foo = Extensions.Foo.{ bar = Some 5; extensions' = Ocaml_protoc_plugin.Extensions.default } in
  let foo = Extensions.Baz'.set foo (Some 8) in
  let foo = Extensions.Baz'.set foo (Some 7) in
  let baz = Extensions.Baz'.get foo in
  print_endline ([%show: Extensions.Foo.t] foo);
  print_endline ([%show: Extensions.Baz.t Ocaml_protoc_plugin.Result.t] baz);
  let () = match baz = Ok (Some 7) with
    | false -> print_endline "Failed. Not equal"
    | true -> ()
  in
  ();
  [%expect {|
    { bar = (Some 5); extensions' = (128, (Field.Varint 7L)) }
    Ok (Some 7) |}]

let%expect_test _ =
  let foo = Extensions.Foo.{ bar = Some 5; extensions' = Ocaml_protoc_plugin.Extensions.default } in
  let foo = Extensions.Baz'.set foo (Some 8) in
  let foo = Extensions.Baz'.set foo (Some 0) in
  let foo = Extensions.B2.set foo ([6;7;8]) in
  let foo = Extensions.B2.set foo ([]) in
  print_endline ([%show: Extensions.Foo.t] foo);
  ();
  [%expect {|
    { bar = (Some 5); extensions' = (128, (Field.Varint 0L)) } |}]

let%expect_test _ =
  let foo = Extensions.Foo.{ bar = Some 5; extensions' = Ocaml_protoc_plugin.Extensions.default } in
  let foo = Extensions.Baz'.set foo (Some 7) in
  let foo' =
    Extensions.Foo.to_proto foo
    |> Ocaml_protoc_plugin.Writer.contents
    |> Ocaml_protoc_plugin.Reader.create
    |> Extensions.Foo.from_proto
    |> Ocaml_protoc_plugin.Result.get ~msg:"Failed decoding"
  in
  let baz = Extensions.Baz'.get foo' in
  print_endline ([%show: Extensions.Baz.t Ocaml_protoc_plugin.Result.t] baz);
  let () = match baz = Ok (Some 7) with
    | false -> print_endline "Failed. Not equal"
    | true -> ()
  in
  ();
  [%expect {| Ok (Some 7) |}]

let%expect_test _ =
  let v = [6;7;8;9] in
  let foo = Extensions.Foo.{ bar = Some 5; extensions' = Ocaml_protoc_plugin.Extensions.default } in
  let foo = Extensions.R_baz.set foo v in
  let foo' =
    Extensions.Foo.to_proto foo
    |> Ocaml_protoc_plugin.Writer.contents
    |> Ocaml_protoc_plugin.Reader.create
    |> Extensions.Foo.from_proto
    |> Ocaml_protoc_plugin.Result.get ~msg:"Failed decoding"
  in
  let r_baz = Extensions.R_baz.get foo' in
  print_endline ([%show: Extensions.R_baz.t Ocaml_protoc_plugin.Result.t] r_baz);
  let () = match r_baz = Ok v with
    | false -> print_endline "Failed. Not equal"
    | true -> ()
  in
  ();
  [%expect {| Ok [6; 7; 8; 9] |}]

let%expect_test _ =
  let foo = Extensions.Foo.{ bar = Some 5; extensions' = Ocaml_protoc_plugin.Extensions.default } in
  print_endline ([%show: Extensions.Foo.t] foo);

  let foo = Extensions.A.set foo (Some 7) in
  Printf.printf "Set A = Some 7\n";
  print_endline ([%show: Extensions.Foo.t] foo);

  let foo = Extensions.A.set foo None in
  Printf.printf "Set A = None\n";
  print_endline ([%show: Extensions.Foo.t] foo);

  let foo = Extensions.B.set foo 15 in
  Printf.printf "Set B = 15: %d\n" (Extensions.B.get foo |> Ocaml_protoc_plugin.Result.get ~msg:"No Value");
  print_endline ([%show: Extensions.Foo.t] foo);

  let foo = Extensions.B.set foo 13 in
  Printf.printf "Set B = 13: %d\n" (Extensions.B.get foo |> Ocaml_protoc_plugin.Result.get ~msg:"No Value");
  print_endline ([%show: Extensions.Foo.t] foo);

  let foo = Extensions.B.set foo 0 in
  Printf.printf "Set B = 0: %d\n" (Extensions.B.get foo |> Ocaml_protoc_plugin.Result.get ~msg:"No Value");
  print_endline ([%show: Extensions.Foo.t] foo);
  ();
  [%expect {|
    { bar = (Some 5); extensions' =  }
    Set A = Some 7
    { bar = (Some 5); extensions' = (131, (Field.Varint 7L)) }
    Set A = None
    { bar = (Some 5); extensions' =  }
    Set B = 15: 15
    { bar = (Some 5); extensions' = (132, (Field.Varint 15L)) }
    Set B = 13: 13
    { bar = (Some 5); extensions' =  }
    Set B = 0: 0
    { bar = (Some 5); extensions' = (132, (Field.Varint 0L)) } |}]
