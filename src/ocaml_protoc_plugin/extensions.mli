type t = (int * Field.t) list

val to_yojson : t -> Yojson.t
val yojson_of_t : t -> Yojson.t
val of_yojson : Yojson.t -> t
val t_of_yojson : Yojson.t -> t

val default : t
val pp : Format.formatter -> t -> unit
val show : t -> string
val equal : t -> t -> bool
val compare : t -> t -> int
val get : ('b -> 'b, 'b) Deserialize.S.compound_list -> t -> 'b Result.t
val set : ('a -> Writer.t, Writer.t) Spec.Serialize.compound_list -> t -> 'a -> t
