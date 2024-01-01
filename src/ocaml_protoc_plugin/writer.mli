type t

(** Create a new writer *)
val init : unit -> t

(** Construct a writer from a list of fields *)
val of_list: (int * Field.t) list -> t

(** Get the protobuf encoded contents of the writer *)
val contents : t -> string

(**/**)
val write_varint : bytes -> offset:int -> int64 -> int
val write_fixed32 : bytes -> offset:int -> Int32.t -> int
val write_fixed64 : bytes -> offset:int -> Int64.t -> int
val write_length_delimited :
  bytes -> offset:int -> src:string -> src_pos:int -> len:int -> int
val write_field : t -> int -> Field.t -> unit

val add_field : t -> Field.t -> unit
val add_length_delimited_field_header : t -> int -> (unit -> unit)
val dump : t -> unit
(**/**)
