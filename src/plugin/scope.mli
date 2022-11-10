type t
val init : Spec.Descriptor.Google.Protobuf.FileDescriptorProto.t list -> t

val for_descriptor: t -> Spec.Descriptor.Google.Protobuf.FileDescriptorProto.t -> t

(** Push an identifier to the current scope *)
val push : t -> string -> t

(** Get the module name of a proto file *)
val module_name_of_proto: string -> string

(** The import module name - Must be globally unique *)
val import_module_name: string

(** Get the ocaml name of the given proto type name, based on the current scope *)
val get_scoped_name : ?postfix:string -> t -> string option -> string

(** Get the ocaml name of the given proto type name, based on the current scope *)
val get_name : t -> string -> string

(** Get the ocaml name of the given proto type name, based on the current scope *)
val get_name_exn : t -> string option -> string

(** Get the type of the curren scope *)
val get_current_scope : t -> string

(**  Get the path of the given scpoe *)
val get_current_proto_path : t -> string option

(** Tell if the type pointed to by the current scope is part of a cycle. *)
val is_cyclic: t -> bool
