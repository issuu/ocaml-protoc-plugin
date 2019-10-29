type t
val init : Spec.Descriptor.Google.Protobuf.FileDescriptorProto.t list -> t
val for_descriptor: t -> Spec.Descriptor.Google.Protobuf.FileDescriptorProto.t -> t
val get_message_name: t -> string option -> string
val push : t -> string -> t
val get_scoped_name : ?postfix:string -> t -> string option -> string
val get_current_scope : t -> string
val module_name_of_proto: string -> string
val is_cyclic: t -> bool
val is_recursive: t -> bool
