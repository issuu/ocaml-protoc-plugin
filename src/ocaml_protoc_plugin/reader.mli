type t

(** Create a reader from a string, to be used when deserializing a protobuf type *)
val create : ?offset:int -> ?length:int -> string -> t

(**/**)
val has_more : t -> bool
val read_varint : t -> (Field.t, [> `Premature_end_of_input ]) result
val read_length_delimited :
  t -> (Field.t, [> `Premature_end_of_input ]) result
val read_fixed32 : t -> (Field.t, [> `Premature_end_of_input ]) result
val read_fixed64 : t -> (Field.t, [> `Premature_end_of_input ]) result
val read_field : t -> (int * Field.t) Result.t
(**/**)
