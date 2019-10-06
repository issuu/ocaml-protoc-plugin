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


let%expect_test _ =
  let module T = Recursive.StdTree in

  let rec add v = function
    | None -> Some T.{ left = None; value = v; right = None}
    | Some T.{ left; value; right} when v < value ->
      Some T.{ left = add v left; value; right }
    | Some T.{ left; value; right} when v > value ->
      Some T.{ left = left; value; right = add v right; }
    | x -> x
  in
  let rec elements = function
    | None -> 0
    | Some T.{left; right; _} -> 1 + elements left + elements right
  in
  let rec depth = function
    | None -> 0
    | Some T.{ left; right; _} ->
      max (depth left) (depth right) + 1
  in

  (* Protoc cannot handle nested structure with a depth > 101. *)
  let t =
    List.init 10000 (fun i -> i lxor 0x57c)
    |> List.fold_left (fun acc i -> add i acc) None
    |> fun t -> T.{ left = t; value = 10000; right = None }
  in
  Printf.printf "Elements: %d\n" (elements (Some t));
  Printf.printf "Depth: %d\n" (depth (Some t));

  Test_lib.test_encode ~protoc:false (module T) t;
  [%expect {|
    Elements: 10001
    Depth: 200 |}]
