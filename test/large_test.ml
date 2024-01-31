open Large

let%expect_test "Test very large message type" =
  let large = Large.make ~x7:7 () in
  Test_lib.test_encode (module Large) large;
  ();
  [%expect {|
    x7: 7 |}]
