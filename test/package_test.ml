open Package
let%expect_test _ =
  let module T = Package.A.B.M in
  let t = 7 in
  Test_lib.test_encode (module T) t;
  [%expect {|
    i: 7 |}]
