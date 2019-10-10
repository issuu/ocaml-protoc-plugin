(* Mimic base Option type *)
type 'a t = 'a option

let value ~default = function
  | None -> default
  | Some v -> v

let value_exn ?(message="Option is None") = function
  | None -> failwith message
  | Some v -> v

let value_map ~default ~f = function
  | None -> default
  | Some v -> f v

let map ~f = function
  | Some v -> Some (f v)
  | None -> None

let iter ~f = function
  | Some v -> f v
  | None -> ()
