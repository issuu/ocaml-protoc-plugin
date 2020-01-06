open Extensions

(* Set extensions *)
let _ =
  let foo = Foo.{ i = 31; extensions' = Ocaml_protoc_plugin.Extensions.default } in
  let foo_with_bar = Bar.set foo (Some 42) in
  let foo_with_baz = Baz.set foo (Some "Test String") in
  let foo_with_bar_baz = Baz.set foo_with_bar (Some "Test String") in

  (* Get extensions *)
  let open Ocaml_protoc_plugin.Result in
  Bar.get foo_with_bar >>= fun bar ->
  Baz.get foo_with_baz >>= fun baz ->
  assert (bar = Some 42);
  assert (baz = Some "Test String");
  Bar.get foo_with_bar_baz >>= fun bar' ->
  Baz.get foo_with_bar_baz >>= fun baz' ->
  assert (bar' = Some 42);
  assert (baz' = Some "Test String");
  return ()
