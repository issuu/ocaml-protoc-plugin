type t

(** Create a new writer *)
val init : unit -> t

(** Get the protobuf encoded contents of the writer *)
val contents : t -> string

(**/**)
val write_varint : bytes -> offset:int -> int64 -> int
val write_varint_reference : bytes -> offset:int -> int64 -> int
val write_varint_unboxed : bytes -> offset:int -> int -> int
val write_varint_unboxed_reference : bytes -> offset:int -> int -> int
val write_fixed32 : bytes -> offset:int -> Int32.t -> int
val write_fixed64 : bytes -> offset:int -> Int64.t -> int
val write_string : bytes -> offset:int -> string -> int
val write_length_delimited :
  bytes -> offset:int -> src:string -> src_pos:int -> len:int -> int
val write_field : t -> int -> Field.t -> unit
val write_length_delimited_value : write:('a -> t -> unit) -> 'a -> t -> unit
val of_list: (int * Field.t) list -> t
val dump : t -> unit
val unused : t -> int
val varint_size: int -> int
val write_value: size:int -> writer:(Bytes.t -> offset:int -> 'a -> int) -> 'a -> t -> unit
(**/**)
