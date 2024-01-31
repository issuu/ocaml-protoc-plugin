module C = Spec.Deserialize.C

val deserialize: (int * int) list ->
  ('constr, (int * Field.t) list -> 'a) Spec.Deserialize.compound_list ->
  'constr -> Reader.t -> 'a
