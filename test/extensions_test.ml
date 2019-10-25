open Extensions

let%expect_test _ =
  let foo = Extensions.Foo.{ bar = Some 5; extensions = Ocaml_protoc_plugin.Extensions.default } in
  let foo = Extensions.Baz.set foo (Some 7) in
  let baz = Extensions.Baz.get foo in
  print_endline ([%show: Extensions.Baz.t Ocaml_protoc_plugin.Result.t] baz);
  let () = match baz = Ok (Some 7) with
    | false -> print_endline "Failed. Not equal"
    | true -> ()
  in
  ();
  [%expect {| Ok (Some 7) |}]

let%expect_test _ =
  let foo = Extensions.Foo.{ bar = Some 5; extensions = Ocaml_protoc_plugin.Extensions.default } in
  let foo = Extensions.Baz.set foo (Some 8) in
  let foo = Extensions.Baz.set foo (Some 7) in
  let baz = Extensions.Baz.get foo in
  print_endline ([%show: Extensions.Baz.t Ocaml_protoc_plugin.Result.t] baz);
  let () = match baz = Ok (Some 7) with
    | false -> print_endline "Failed. Not equal"
    | true -> ()
  in
  ();
  [%expect {| Ok (Some 7) |}]
