open Core

let%expect_test _ =
  let module T = Primitive_types.Types in
  let t =
    T.
      {
        int64 = 1;
        sint64 = 2;
        uint64 = 3;
        int32 = 4;
        sint32 = 5;
        uint32 = 6;
        double = 7.1;
        float = 8.1;
        fixed64 = 9;
        fixed32 = 10;
        sfixed64 = 11;
        sfixed32 = 12;
        bool = true;
        string = "string";
        bytes = Bytes.of_string "bytes";
      }
  in
  Test_lib.test_encode "primitive_types.proto" (module T) t;
  [%expect
    {|
    int64: 1
    sint64: 2
    uint64: 3
    int32: 4
    sint32: 5
    uint32: 6
    double: 7.1
    float: 8.1
    fixed64: 9
    fixed32: 10
    sfixed64: 11
    sfixed32: 12
    bool: true
    string: "string"
    bytes: "bytes"

    Decode failed |}]

let%expect_test _ =
  let module T = Primitive_types.Int64 in
  let t = T.{i1 = 0; i2 = -1; i3 = -2; i4 = 8589934592} in
  Test_lib.test_encode "primitive_types.proto" (module T) t;
  [%expect {|
    i2: -1
    i3: -2
    i4: 8589934592

    Decode failed |}]

let%expect_test _ =
  let module T = Primitive_types.Int32 in
  let t = T.{i1 = 2147483647; i2 = -2147483646; i3 = -1; i4 = -2} in
  Test_lib.test_encode "primitive_types.proto" (module T) t;
  [%expect
    {|
    i1: 2147483647
    i2: -2147483646
    i3: -1
    i4: -2

    Decode failed |}]
