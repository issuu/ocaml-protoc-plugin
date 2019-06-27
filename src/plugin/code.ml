open Core

type t = {
  mutable indent: string;
  mutable code: string list;
}
let init () = {
  indent = "";
  code = [];
}

let incr t =
  t.indent <- "  " ^ t.indent

let decr t =
  t.indent <- String.chop_prefix_exn ~prefix:"  " t.indent

let emit t indent fmt =
  let emit s =
    match indent with
    | `Begin ->
      t.code <- (t.indent ^ s) :: t.code;
      incr t;
      | `None->
        t.code <- (t.indent ^ s) :: t.code
      | `End ->
        decr t;
        t.code <- (t.indent ^ s) :: t.code
      | `EndBegin ->
        t.code <- (String.chop_prefix_exn ~prefix:"  " t.indent ^ s) :: t.code
  in
  Printf.ksprintf emit fmt

let append t code =
    List.iter ~f:(emit t `None "%s") (code.code |> List.rev)

let dump t =
  List.iter ~f:(eprintf "%s\n") (List.rev t.code);
  ()
