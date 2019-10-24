type t = Field.t list
let pp : Format.formatter -> t -> unit = fun fmt -> Format.pp_print_list Field.pp fmt
let show : t -> string = Format.asprintf "%a" pp
let equal _ _ = true
let compare _ _ = 0

let get: ('a -> 'b, 'b) Deserialize.S.compound_list -> t -> 'b = fun _spec _t -> failwith "Oops"
let set: ('a -> 'b, 'b) Serialize.S.compound_list -> t -> 'b -> t = fun _spec _t _v -> failwith "Oops"
