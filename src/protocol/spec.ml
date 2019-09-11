type field =
  | Varint of int (* int32, int64, uint32, uint64, sint32, sint64, bool, enum *)
  | Fixed_64_bit of Int64.t (* fixed64, sfixed64, double *)
  | Length_delimited of string (* string, bytes, embedded messages, packed repeated fields *)
  | Fixed_32_bit of Int32.t

(* fixed32, sfixed32, float *)

type error =
  [ `Wrong_field_type of string * field
  | `Illegal_value of string * field
  | `Not_implemented ]

type 'a deser_result = ('a, error) result

type _ protobuf_type =
  | Double : float protobuf_type
  | Float : float protobuf_type
  | Int32 : int protobuf_type
  | Int64 : int protobuf_type
  | UInt32 : int protobuf_type
  | UInt64 : int protobuf_type
  | SInt32 : int protobuf_type
  | SInt64 : int protobuf_type
  | Fixed32 : int protobuf_type (* unsigned *)
  | Fixed64 : int protobuf_type
  | SFixed32 : int protobuf_type
  | SFixed64 : int protobuf_type
  | Bool : bool protobuf_type
  | String : string protobuf_type
  | Bytes : bytes protobuf_type
  | Message : ('a -> string) -> 'a option protobuf_type
  | Enum : ('a -> int) -> 'a protobuf_type
  | Repeated : 'a protobuf_type -> 'a list protobuf_type
  | Oneof : ('a -> int * field) -> 'a protobuf_type

type _ deser =
  | Double : float deser
  | Float : float deser
  | Int32 : int deser
  | Int64 : int deser
  | UInt32 : int deser
  | UInt64 : int deser
  | SInt32 : int deser
  | SInt64 : int deser
  | Fixed32 : int deser (* unsigned *)
  | Fixed64 : int deser
  | SFixed32 : int deser
  | SFixed64 : int deser
  | Bool : bool deser
  | String : string deser
  | Bytes : bytes deser
  | Message : (string -> 'a deser_result) -> 'a option deser
  | Enum : (int -> 'a deser_result) -> 'a deser
  | Repeated : 'a deser -> 'a list deser

(* Oneofs currently not types here *)
