open StdLabels

let test_signed (type t) ~(create : int -> t) (module T : Test_lib.T with type t = t) =
  Printf.printf "Test %s\n%!" (T.name' ());
  let values = [-1073741823; -2; -1; 0; 1; 2; 1073741823] in
  List.iter
    ~f:(fun v -> Test_lib.test_encode (module T) (create v))
    values

let test_unsigned (type t) ~(create : int -> t) (module T : Test_lib.T with type t = t) =
  Printf.printf "Test %s\n%!" (T.name' ());
  let values = [0; 1; 2; 2147483647] in
  List.iter
    ~f:(fun v -> Test_lib.test_encode (module T) (create v))
    values

let%expect_test _ =
  let module T = Int_types.SInt64 in
  let create i = Int_types.SInt64.{i} in
  test_signed ~create (module T);
  [%expect {|
    Test Int_types.SInt64
    i: -1073741823
    i: -2
    i: -1
    i: 1
    i: 2
    i: 1073741823 |}]

let%expect_test _ =
  let module T = Int_types.SInt32 in
  let create i = Int_types.SInt32.{i} in
  test_signed ~create (module T);
  [%expect {|
    Test Int_types.SInt32
    i: -1073741823
    i: -2
    i: -1
    i: 1
    i: 2
    i: 1073741823 |}]

let%expect_test _ =
  let module T = Int_types.Int64 in
  let create i = Int_types.Int64.{i} in
  test_signed ~create (module T);
  [%expect {|
    Test Int_types.Int64
    i: -1073741823
    i: -2
    i: -1
    i: 1
    i: 2
    i: 1073741823 |}]

let%expect_test _ =
  let module T = Int_types.Int32 in
  let create i = Int_types.Int32.{i} in
  test_signed ~create (module T);
  [%expect
    {|
    Test Int_types.Int32
    i: -1073741823
    i: -2
    i: -1
    i: 1
    i: 2
    i: 1073741823 |}]

let%expect_test _ =
  let module T = Int_types.UInt64 in
  let create i = Int_types.UInt64.{i} in
  test_unsigned ~create (module T);
  [%expect {|
    Test Int_types.UInt64
    i: 1
    i: 2
    i: 2147483647 |}]

let%expect_test _ =
  let module T = Int_types.UInt32 in
  let create i = Int_types.UInt32.{i} in
  test_unsigned ~create (module T);
  [%expect {|
    Test Int_types.UInt32
    i: 1
    i: 2
    i: 2147483647 |}]
