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
        float = 8.0;
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
    float: 8
    fixed64: 9
    fixed32: 10
    sfixed64: 11
    sfixed32: 12
    bool: true
    string: "string"
    bytes: "bytes" |}]
