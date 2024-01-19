type t = (int * Field.t) list
val default : t
val pp : Format.formatter -> t -> unit
val show : t -> string
val equal : t -> t -> bool
val compare : t -> t -> int
val get : ('a -> t -> 'a, t -> 'a) Spec.Deserialize.compound_list -> t -> 'a
val set : ('a -> Writer.t, Writer.t) Spec.Serialize.compound_list -> t -> 'a -> t
