let%expect_test _ =
  let module T = Map.Test in
  let t = T.{ m = [ 1, "1"; 2, "2"; 3, "3" ] } in
  Test_lib.test_encode (module T) t;
  [%expect {|
    m {;  key: 1;  value: "1"; }; m {;  key: 2;  value: "2"; }; m {;  key: 3;  value: "3"; }; |}]


let%expect_test _ =
  let module T = Map.Two in
  let t = T.{ m = [ 1, "1"; 2, "2"; 3, "3" ];
              n = [ 1, 1.0; 2, 2.0; 3, 3.0 ]} in
  Test_lib.test_encode (module T) t;
  [%expect {|
    m {;  key: 1;  value: "1"; }; m {;  key: 2;  value: "2"; }; m {;  key: 3;  value: "3"; }; n {;  key: 1;  value: 1; }; n {;  key: 2;  value: 2; }; n {;  key: 3;  value: 3; }; |}]


let%expect_test _ =
  let module T = Map.Map_message in
  let t = T.{ m = [ 1, Some (T.Inner.{ i = 1});
                    2, Some (T.Inner.{ i = 1});
                    3, Some (T.Inner.{ i = 1});
                    4, Some (T.Inner.{ i = 1}); ] }
  in
  Test_lib.test_encode (module T) t;
  [%expect {|
    m {;  key: 1;  value {;  i: 1;  }; }; m {;  key: 2;  value {;  i: 1;  }; }; m {;  key: 3;  value {;  i: 1;  }; }; m {;  key: 4;  value {;  i: 1;  }; }; |}]
