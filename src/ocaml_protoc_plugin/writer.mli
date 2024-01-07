type t

type mode = Balanced | Speed | Space

(** Create a new writer to hold serialized data.
    The writer also controls how data is serialized and allows for different modes of operation though the [mode] parameter:
    [Balanced]:: Serializes data in a strictly compliant mode. Balance space and speed.
    [Speed]:: Applies optimization which is exploiting the protobuf wire format (but not violating it). Its believed to be safe, but may confuse other protobuf deserializers. The optimization mainly speeds up serialization of large recursive message types. Resulting protobuf serialization is slightly larger than needed, but is comparable to [Space] mode in terms of extra memory used while serialization.
    [Space]:: Limits space overhead (space waste) caused when allocated datablocks cannot be fully filled. The mode causes multiple data copies while serializing to avoid space overhead. This is the default.

    [block_size] controls the minimum size of block allocation. Setting this to zero will significantly slow down serialization but reduce space overhead. Setting a high value may cause more space overhead, esp. for recursive message structures. The default is to allocate block of size 256.
*)
val init: ?mode:mode -> ?block_size:int -> unit -> t

(** Get the protobuf encoded contents of the writer *)
val contents : t -> string

(**/**)
val varint_size : int -> int
val write_varint : bytes -> offset:int -> int64 -> int
val write_varint_unboxed : bytes -> offset:int -> int -> int
val write_fixed32 : bytes -> offset:int -> Int32.t -> int
val write_fixed64 : bytes -> offset:int -> Int64.t -> int
val write_string : bytes -> offset:int -> string -> int
val write_length_delimited : bytes -> offset:int -> src:string -> src_pos:int -> len:int -> int
val write_field : t -> int -> Field.t -> unit
val write_length_delimited_value : write:(t -> 'a -> 'b) -> 'a -> t -> unit

(** Construct a writer from a field list *)
val of_list: (int * Field.t) list -> t

(** Dump contents of the writer to stdout *)
val dump : t -> unit

val unused_space : t -> int
val write_value: size:int -> writer:(Bytes.t -> offset:int -> 'a -> int) -> 'a -> t -> unit
(**/**)
