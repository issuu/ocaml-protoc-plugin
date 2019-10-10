open StdLabels

type t = {
  mutable indent : string;
  mutable code : string list;
}

let init () = {indent = ""; code = []}
let incr t = t.indent <- "  " ^ t.indent
let decr t = t.indent <- String.sub ~pos:0 ~len:(String.length t.indent - 2) t.indent

let emit t indent fmt =
  let emit s =
    match indent with
    | `Begin ->
      t.code <- (t.indent ^ s) :: t.code;
      incr t
    | `None -> t.code <- (t.indent ^ s) :: t.code
    | `End ->
      decr t;
      t.code <- (t.indent ^ s) :: t.code
    | `EndBegin ->
      decr t;
      t.code <- (t.indent ^ s) :: t.code;
      incr t
  in
  Printf.ksprintf emit fmt

let append t code = List.iter ~f:(emit t `None "%s") (code.code |> List.rev)

let contents t =
  List.map ~f:(Printf.sprintf "%s") (List.rev t.code)
  |> String.concat ~sep:"\n"
