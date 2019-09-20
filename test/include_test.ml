open Base

let%expect_test _ =
  let module T = Include.I in
  let t = T.{ enum = Enum.Message.E.B;
              m = Some (Package.A.B.M.{ i = 3; });
              o = Some (Enum.Outside.{ enum = Enum.E1.C; });
              c = Some (Include.X.Inner.{ i = 7; });
            } in
  Test_lib.test_encode (module T) t;
  [%expect {|
    enum: B;m {; i: 3;};o {; enum: C;};c {; i: 7;}; |}]
