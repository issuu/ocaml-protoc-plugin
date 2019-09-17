
let%expect_test _ =
  let module T = Map.Test in
  let elem key value = T.MEntry.{ key; value } in
  let t = T.{ m = [elem 1 "x"; elem 2 "y"]} in
  Test_lib.test_encode "map.proto" (module T) t;
  [%expect {|
    m {;  key: 1;  value: "x";};m {;  key: 2;  value: "y";}; |}]
