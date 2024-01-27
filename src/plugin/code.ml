open StdLabels

type t = {
  mutable indent : string;
  mutable code : string list;
}

let init () = {indent = ""; code = []}
let incr t = t.indent <- "  " ^ t.indent
let decr t =
  match String.length t.indent >= 2 with
  | true ->
    t.indent <- String.sub ~pos:0 ~len:(String.length t.indent - 2) t.indent
  | false -> failwith "Cannot decr indentation level at this point"

let emit t indent fmt =
  let trim_end ~char s =
    let len = String.length s in
    let rcount s =
      let rec inner = function
        | 0 -> len
        | n when s.[n - 1] = char -> inner (n - 1)
        | n -> len - n
      in
      inner len
    in
    match rcount s with
    | 0 -> s
    | n -> String.sub ~pos:0 ~len:(String.length s - n) s
  in
  let prepend s =
    match String.split_on_char ~sep:'\n' s with
    | line :: lines ->
      t.code <- (trim_end ~char:' ' (t.indent ^ line)) :: t.code;
      incr t;
      List.iter lines ~f:(fun line -> t.code <- (trim_end ~char:' ' (t.indent ^ line)) :: t.code);
      decr t;
    | [] -> ()
  in
  let emit s =
    match indent with
    | `Begin ->
      prepend s;
      incr t
    | `None ->
      prepend s
    | `End ->
      decr t;
      prepend s
    | `EndBegin ->
      decr t;
      prepend s;
      incr t
  in
  Printf.ksprintf emit fmt

let append t code = List.iter ~f:(emit t `None "%s") (code.code |> List.rev)

let contents t =
  List.map ~f:(Printf.sprintf "%s") (List.rev t.code)
  |> String.concat ~sep:"\n"
