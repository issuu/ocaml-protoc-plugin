open Singleton_record

let%expect_test _ =
  let module T = Singleton_record.Test in
  let t = `J "Test" in
  Test_lib.test_encode (module T) t;
  [%expect {|
    j: "Test" |}]

let%expect_test _ =
  let module T = Singleton_record.Test2 in
  let t = `F3 "Test" in
  Test_lib.test_encode (module T) t;
  [%expect {|
    f3: "Test" |}]

let%expect_test _ =
  let module T = Singleton_record.Test3 in
  let t = 7 in
  Test_lib.test_encode (module T) t;
  [%expect {|
    x1: 7 |}]

let%expect_test _ =
  let module T = Singleton_record.Test4 in
  let t = Some (`J "test") in
  Test_lib.test_encode (module T) t;
  [%expect {|
    t {
      j: "test"
    } |}]

let%expect_test _ =
  let module T = Singleton_record.Test5 in
  let t = Some T.M.E.B in
  Test_lib.test_encode (module T) t;
  [%expect {|
    m {
      enum: B
    } |}]
