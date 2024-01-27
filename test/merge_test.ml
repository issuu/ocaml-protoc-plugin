open !StdLabels
open Merge.Merge

let test_merge (type t) (module T: Test_lib.T with type t = t) (init : t) (ts: t list) =
  let open Ocaml_protoc_plugin in

  let writer = Writer.init () in
  let expect =
    List.fold_left ~init ~f:(fun acc t ->
      Printf.printf "%s\n" (T.show t);
      let _ = T.to_proto' writer t in
      T.merge acc t
    ) ts
  in
  let merged = T.from_proto (Reader.create (Writer.contents writer)) |> Result.get ~msg:"Unable to decode merged messages" in
  Printf.printf "Merged: %s\n" (T.show merged);
  let () = match merged = expect with
    | false ->
      Printf.printf "Merge results not equal\n";
      Printf.printf "Expected: %s\n" (T.show expect);
    | true -> ()
  in
  ()

let%expect_test "merge int" =
  (* Create a set of tests, each expanding on the previous *)
  (* And we should extend test_encode to verify merge for all message types *)
  (* But in this test we want to explicitly test it *)
  (* Also for merging multiple messages *)

  let t1 = T.make ~a:5 () in
  let t2 = T.make ~a:7 () in
  test_merge (module T) (T.make ()) [t1; t2];
  [%expect {|
    { a = 5; b = []; c = []; d = None; o = `not_set }
    { a = 7; b = []; c = []; d = None; o = `not_set }
    Merged: { a = 7; b = []; c = []; d = None; o = `not_set } |}]

let%expect_test "merge int" =
  (* Create a set of tests, each expanding on the previous *)
  (* And we should extend test_encode to verify merge for all message types *)
  (* But in this test we want to explicitly test it *)
  (* Also for merging multiple messages *)

  let t1 = T.make ~b:[1;2;3] () in
  let t2 = T.make ~b:[4;5;6] () in
  let t3 = T.make ~b:[7;8;9] () in
  test_merge (module T) (T.make ()) [t1; t2; t3];
  [%expect {|
    { a = 0; b = [1; 2; 3]; c = []; d = None; o = `not_set }
    { a = 0; b = [4; 5; 6]; c = []; d = None; o = `not_set }
    { a = 0; b = [7; 8; 9]; c = []; d = None; o = `not_set }
    Merged: { a = 0; b = [1; 2; 3; 4; 5; 6; 7; 8; 9]; c = []; d = None; o = `not_set } |}]

let%expect_test "merge string" =
  (* Create a set of tests, each expanding on the previous *)
  (* And we should extend test_encode to verify merge for all message types *)
  (* But in this test we want to explicitly test it *)
  (* Also for merging multiple messages *)

  let t1 = T.make ~c:["1";"2";"3"] () in
  let t2 = T.make ~c:["4";"5";"6"] () in
  let t3 = T.make ~c:["7";"8";"9"] () in
  test_merge (module T) (T.make ()) [t1; t2; t3];
  [%expect {|
    { a = 0; b = []; c = ["1"; "2"; "3"]; d = None; o = `not_set }
    { a = 0; b = []; c = ["4"; "5"; "6"]; d = None; o = `not_set }
    { a = 0; b = []; c = ["7"; "8"; "9"]; d = None; o = `not_set }
    Merged: { a = 0; b = []; c = ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"]; d = None;
      o = `not_set } |}]

let%expect_test "merge message" =
  (* Create a set of tests, each expanding on the previous *)
  (* And we should extend test_encode to verify merge for all message types *)
  (* But in this test we want to explicitly test it *)
  (* Also for merging multiple messages *)

  let t11 = T.make ~a:1 ~b:[1;2;3] () in
  let t12 = T.make ~a:2 ~b:[4;5;6] () in
  let t13 = T.make ~a:3 ~b:[7;8;9] () in
  let t1 = T.make ~a:6 ~d:t11 () in
  let t2 = T.make ~a:7 ~d:t12 () in
  let t3 = T.make ~a:8 ~d:t13 () in

  test_merge (module T) (T.make ()) [t1; t2; t3];
  [%expect {|
    { a = 6; b = []; c = [];
      d = (Some { a = 1; b = [1; 2; 3]; c = []; d = None; o = `not_set });
      o = `not_set }
    { a = 7; b = []; c = [];
      d = (Some { a = 2; b = [4; 5; 6]; c = []; d = None; o = `not_set });
      o = `not_set }
    { a = 8; b = []; c = [];
      d = (Some { a = 3; b = [7; 8; 9]; c = []; d = None; o = `not_set });
      o = `not_set }
    Merged: { a = 8; b = []; c = [];
      d =
      (Some { a = 3; b = [1; 2; 3; 4; 5; 6; 7; 8; 9]; c = []; d = None;
              o = `not_set });
      o = `not_set } |}]

let%expect_test "merge last oneof" =
  (* Create a set of tests, each expanding on the previous *)
  (* And we should extend test_encode to verify merge for all message types *)
  (* But in this test we want to explicitly test it *)
  (* Also for merging multiple messages *)

  let t1 = T.make ~o:(`I 5) () in
  let t2 = T.make ~o:(`J "7") () in
  test_merge (module T) (T.make ()) [t1; t2];
  [%expect {|
    { a = 0; b = []; c = []; d = None; o = `I (5) }
    { a = 0; b = []; c = []; d = None; o = `J ("7") }
    Merged: { a = 0; b = []; c = []; d = None; o = `J ("7") } |}]

let%expect_test "merge message oneof" =
  (* Create a set of tests, each expanding on the previous *)
  (* And we should extend test_encode to verify merge for all message types *)
  (* But in this test we want to explicitly test it *)
  (* Also for merging multiple messages *)

  let t11 = T.make ~a:1 ~b:[1;2;3] () in
  let t12 = T.make ~a:2 ~b:[4;5;6] () in
  let t13 = T.make ~a:3 ~b:[7;8;9] () in

  let t1 = T.make ~o:(`K t11) () in
  let t2 = T.make ~o:(`K t12) () in
  let t3 = T.make ~o:(`K t13) () in
  test_merge (module T) (T.make ()) [t1; t2; t3];
  [%expect {|
    { a = 0; b = []; c = []; d = None;
      o = `K ({ a = 1; b = [1; 2; 3]; c = []; d = None; o = `not_set }) }
    { a = 0; b = []; c = []; d = None;
      o = `K ({ a = 2; b = [4; 5; 6]; c = []; d = None; o = `not_set }) }
    { a = 0; b = []; c = []; d = None;
      o = `K ({ a = 3; b = [7; 8; 9]; c = []; d = None; o = `not_set }) }
    Merged: { a = 0; b = []; c = []; d = None;
      o = `K ({ a = 3; b = [7; 8; 9]; c = []; d = None; o = `not_set }) }
    Merge results not equal
    Expected: { a = 0; b = []; c = []; d = None;
      o =
      `K ({ a = 3; b = [1; 2; 3; 4; 5; 6; 7; 8; 9]; c = []; d = None;
            o = `not_set })
      } |}]
