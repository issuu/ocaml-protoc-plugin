open Base

let () = Stdlib.(Printf.eprintf "Module: %s\n" Stdlib.__MODULE__)

let%expect_test _ =
  let module T = Package.A.B.M in
  let t = T.{i = 7} in
  Test_lib.test_encode (Test_lib.protofile_of_module Stdlib.__MODULE__) (module T) t;
  [%expect {|
    i: 7; |}]
