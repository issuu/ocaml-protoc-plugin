module type T = sig
  type ('a, 'deser, 'ser) dir
end

module Make(T : T) = struct

  type 'a proto_type = Proto3 | Proto2 of 'a | Required
  type packed = Packed | Not_packed

  type _ spec =
    | Double : float spec
    | Float : float spec

    | Int32 : Int32.t spec
    | UInt32 : Int32.t spec
    | SInt32 : Int32.t spec
    | Fixed32 : Int32.t spec
    | SFixed32 : Int32.t spec

    | Int32_int : int spec
    | UInt32_int : int spec
    | SInt32_int : int spec
    | Fixed32_int : int spec
    | SFixed32_int : int spec

    | UInt64 : Int64.t spec
    | Int64 : Int64.t spec
    | SInt64 : Int64.t spec
    | Fixed64 : Int64.t spec
    | SFixed64 : Int64.t spec

    | UInt64_int : int spec
    | Int64_int : int spec
    | SInt64_int : int spec
    | Fixed64_int : int spec
    | SFixed64_int : int spec

    | Bool : bool spec
    | String : string spec
    | Bytes : bytes spec

    | Enum :  ('a, int -> 'a Result.t, 'a -> int) T.dir -> 'a spec
    | Message : ('a, Reader.t -> 'a Result.t, 'a -> Writer.t) T.dir -> 'a spec

  type _ oneof =
    | Oneof_elem : int * 'b spec * ('a, ('b -> 'a), 'b) T.dir -> 'a oneof

  type _ compound =
    | Basic : int * 'a spec * 'a proto_type -> 'a compound
    | Basic_opt : int * 'a spec -> 'a option compound
    | Repeated : int * 'a spec * packed -> 'a list compound
    | Oneof : ('a, 'a oneof list, 'a -> unit oneof) T.dir -> 'a compound

  type (_, _) compound_list =
    | Nil : ('a, 'a) compound_list
    | Cons : ('a compound) * ('b, 'c) compound_list -> ('a -> 'b, 'c) compound_list

  module C = struct
    let double = Double
    let float = Float
    let int32 = Int32
    let int64 = Int64
    let uint32 = UInt32
    let uint64 = UInt64
    let sint32 = SInt32
    let sint64 = SInt64
    let fixed32 = Fixed32
    let fixed64 = Fixed64
    let sfixed32 = SFixed32
    let sfixed64 = SFixed64

    let int32_int = Int32_int
    let int64_int = Int64_int
    let uint32_int = UInt32_int
    let uint64_int = UInt64_int
    let sint32_int = SInt32_int
    let sint64_int = SInt64_int
    let fixed32_int = Fixed32_int
    let fixed64_int = Fixed64_int
    let sfixed32_int = SFixed32_int
    let sfixed64_int = SFixed64_int

    let bool = Bool
    let string = String
    let bytes = Bytes
    let enum f = Enum f
    let message f = Message f

    let some v = Some v
    let none = None
    let proto2 v = Proto2 v
    let proto2_bytes v = Proto2 (Some (Bytes.of_string v))
    let proto3 = Proto3
    let required = Required

    let repeated (i, s, p) = Repeated (i, s, p)
    let basic (i, s, d) = Basic (i, s, d)
    let basic_opt (i, s) = Basic_opt (i, s)
    let oneof s = Oneof s
    let oneof_elem (a, b, c) = Oneof_elem (a, b, c)

    let packed = Packed
    let not_packed = Not_packed

    let ( ^:: ) a b = Cons (a, b)
    let nil = Nil
  end
end

module Deserialize = Make(struct
    type ('a, 'deser, 'ser) dir = 'deser
  end)

module Serialize = Make(struct
    type ('a, 'deser, 'ser) dir = 'ser
  end)
