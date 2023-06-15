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
  let trim_end s =
    (* Find the length of the string without trailing spaces *)
    let rec inner = function
      | 0 -> 0
      | n when s.[n - 1] = ' ' -> inner (n - 1)
      | n -> n
    in
    String.sub ~pos:0 ~len:(inner (String.length s)) s
  in

  let prepend s =
    String.split_on_char ~sep:'\n' s
    |> List.iter ~f:(fun s -> t.code <- (t.indent ^ (trim_end s)) :: t.code)
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
