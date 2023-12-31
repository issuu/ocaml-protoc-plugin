type t

(** Create a reader from a string, to be used when deserializing a protobuf type *)
val create : ?offset:int -> ?length:int -> string -> t

type boxed = Boxed | Unboxed

(**/**)
val has_more : t -> bool
val to_list : t -> (int * Field.t) list
val read_varint : t -> Field.t
val read_varint_unboxed : t -> Field.t
val read_length_delimited : t -> Field.t
val read_fixed32 : t -> Field.t
val read_fixed64 : t -> Field.t
val read_field_header : t -> (int * int)
val read_field_content : boxed -> int -> t -> Field.t
val read_field : boxed -> t -> (int * Field.t)
(**/**)
