type t = (int * Field.t) list
val default : t
val pp : Format.formatter -> t -> unit
val show : t -> string
val equal : t -> t -> bool
val compare : t -> t -> int

val get: 'a Spec.Deserialize.compound -> t -> 'a
val set: 'a Spec.Serialize.compound -> t -> 'a -> t
