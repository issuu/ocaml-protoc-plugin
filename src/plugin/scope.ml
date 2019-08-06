open Core_kernel

type t = string list

let init () = []

let push t name : t = name :: t

let pop t name : t =
  match t with
  | p :: ps when String.equal p name -> ps
  | [] -> failwith "Cannot pop empty scope"
  | _ -> failwith "Cannot pop wrong scope"

let get_scoped_name t = function
  | Some name -> (
    match String.split ~on:'.' name with
    | "" :: xs ->
        let rec inner = function
          | x :: xs, y :: ys when String.Caseless.equal x y -> inner (xs, ys)
          | xs, _ ->
              List.map ~f:String.capitalize xs
              |> List.map ~f:(sprintf "%s.")
              |> String.concat ~sep:""
              |> sprintf "%st"
        in
        inner (xs, List.rev t)
    | _ -> failwith "Expected name to start with a '.'")
  | None -> failwith "Does not contain a name"

let get_current_scope t = String.concat ~sep:"." (List.rev t)
