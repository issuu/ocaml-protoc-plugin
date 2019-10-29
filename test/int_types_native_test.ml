open StdLabels
open Int_types_native

let test_signed64 (type t) ~(create : Int64.t -> t) (module T : Test_lib.T with type t = t) =
  Printf.printf "Test %s\n%!" (T.name' ());
  let values = [-1073741823L; -2L; -1L; 0L; 1L; 2L; 1073741823L] in
  List.iter
    ~f:(fun v -> Test_lib.test_encode (module T) (create v))
    values

let test_unsigned64 (type t) ~(create : Int64.t -> t) (module T : Test_lib.T with type t = t) =
  Printf.printf "Test %s\n%!" (T.name' ());
  let values = [0L; 1L; 2L; 2147483647L] in
  List.iter
    ~f:(fun v -> Test_lib.test_encode (module T) (create v))
    values

let test_signed32 (type t) ~(create : Int32.t -> t) (module T : Test_lib.T with type t = t) =
  Printf.printf "Test %s\n%!" (T.name' ());
  let values = [-1073741823l; -2l; -1l; 0l; 1l; 2l; 1073741823l] in
  List.iter
    ~f:(fun v -> Test_lib.test_encode (module T) (create v))
    values

let test_unsigned32 (type t) ~(create : Int32.t -> t) (module T : Test_lib.T with type t = t) =
  Printf.printf "Test %s\n%!" (T.name' ());
  let values = [0l; 1l; 2l; 2147483647l] in
  List.iter
    ~f:(fun v -> Test_lib.test_encode (module T) (create v))
    values

let%expect_test _ =
  let module T = Int_types_native.SInt64 in
  let create i = i in
  test_signed64 ~create (module T);
  [%expect {|
    Test int_types_native.int_types_native.SInt64
    i: -1073741823
    i: -2
    i: -1
    i: 1
    i: 2
    i: 1073741823 |}]

let%expect_test _ =
  let module T = Int_types_native.SInt32 in
  let create i = i in
  test_signed32 ~create (module T);
  [%expect {|
    Test int_types_native.int_types_native.SInt32
    i: -1073741823
    i: -2
    i: -1
    i: 1
    i: 2
    i: 1073741823 |}]

let%expect_test _ =
  let module T = Int_types_native.Int64 in
  let create i = i in
  test_signed64 ~create (module T);
  [%expect {|
    Test int_types_native.int_types_native.Int64
    i: -1073741823
    i: -2
    i: -1
    i: 1
    i: 2
    i: 1073741823 |}]

let%expect_test _ =
  let module T = Int_types_native.Int32 in
  let create i = i in
  test_signed32 ~create (module T);
  [%expect
    {|
    Test int_types_native.int_types_native.Int32
    i: -1073741823
    i: -2
    i: -1
    i: 1
    i: 2
    i: 1073741823 |}]

let%expect_test _ =
  let module T = Int_types_native.UInt64 in
  let create i = i in
  test_unsigned64 ~create (module T);
  [%expect {|
    Test int_types_native.int_types_native.UInt64
    i: 1
    i: 2
    i: 2147483647 |}]

let%expect_test _ =
  let module T = Int_types_native.UInt32 in
  let create i = i in
  test_unsigned32 ~create (module T);
  [%expect {|
    Test int_types_native.int_types_native.UInt32
    i: 1
    i: 2
    i: 2147483647 |}]
