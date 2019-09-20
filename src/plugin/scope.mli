type t
val init : Spec.Descriptor.file_descriptor_proto list -> t
val push : t -> string -> t
val pop : t -> string -> t
val get_scoped_name :
  postfix:string -> t -> string option -> string
val get_current_scope : t -> string
val module_name_of_proto: string -> string
