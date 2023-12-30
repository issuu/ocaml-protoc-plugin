type t

(** Create a reader from a string, to be used when deserializing a protobuf type *)
val create : ?offset:int -> ?length:int -> string -> t

(**/**)
val has_more : t -> bool
val to_list : t -> (int * Field.t) list
val read_varint : t -> Field.t
val read_length_delimited : t -> Field.t
val read_fixed32 : t -> Field.t
val read_fixed64 : t -> Field.t
val read_field : t -> (int * Field.t)
(**/**)
