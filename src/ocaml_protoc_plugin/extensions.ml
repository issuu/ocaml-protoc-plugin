open StdLabels
type t = (int * Field.t) list
let default = []
let pp_item fmt (index, field) = Format.fprintf fmt "(%d, %a)" index Field.pp field
let pp : Format.formatter -> t -> unit = fun fmt -> Format.pp_print_list pp_item fmt
let show : t -> string = Format.asprintf "%a" pp
let equal _ _ = true
let compare _ _ = 0


let index_of_spec: type a. a Spec.Serialize.compound -> int = function
  | Basic (index, _, _) -> index
  | Basic_opt (index, _) -> index
  | Basic_req (index, _) -> index
  | Repeated (index, _, _) -> index
  | Oneof _ -> failwith "Oneof fields not allowed in extensions"

let get: type a. a Spec.Deserialize.compound -> t -> a = fun spec t ->
  let writer = Writer.of_list t in
  let reader = Writer.contents writer |> Reader.create in
  Deserialize.deserialize Spec.Deserialize.(Cons (spec, Nil)) (fun a -> a) reader

let set: type a. a Spec.Serialize.compound -> t -> a -> t = fun spec t v ->
  let writer = Writer.init () in
  let writer = Serialize.serialize Spec.Serialize.(Cons (spec, Nil)) writer v in
  let index = index_of_spec spec in
  let fields =
    Writer.contents writer
    |> Reader.create
    |> Reader.to_list
  in
  List.filter ~f:(fun (i, _) -> i != index) t @ fields
