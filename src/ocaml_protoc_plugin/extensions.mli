type t = (int * Field.t) list
val to_yojson :
  t ->
  [> `List of
     [> `List of
        [> `Int of int
        | `List of
          [> `Assoc of (string * [> `Int of int | `String of string ]) list
          | `Intlit of string
          | `String of string ]
          list ]
        list ]
     list ]
val default : t
val pp : Format.formatter -> t -> unit
val show : t -> string
val equal : t -> t -> bool
val compare : t -> t -> int
val get : ('b -> 'b, 'b) Deserialize.S.compound_list -> t -> 'b Result.t
val set : ('a -> Writer.t, Writer.t) Spec.Serialize.compound_list -> t -> 'a -> t
