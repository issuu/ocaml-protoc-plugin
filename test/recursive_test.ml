let%expect_test _ =
  let module T = Recursive.Message in
  let t = T.{ m = Some { m = Some { m = None } }} in
  Test_lib.test_encode (module T) t;
  [%expect {|
    m {
      m {
      }
    } |}]


let%expect_test _ =
  let module T1 = Recursive.Mutual1 in
  let module T2 = Recursive.Mutual2 in
  let t = T1.{ m2 = Some T2.{ m1 = Some T1.{ m2 = Some T2.{ m1 = None }}}} in
  Test_lib.test_encode (module T1) t;
  [%expect {|
    m2 {
      m1 {
        m2 {
        }
      }
    } |}]
