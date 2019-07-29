(** descriptor.proto Pretty Printing *)


(** {2 Formatters} *)

val pp_field_descriptor_proto_label : Format.formatter -> Descriptor_types.field_descriptor_proto_label -> unit 
(** [pp_field_descriptor_proto_label v] formats v *)

val pp_field_descriptor_proto_type : Format.formatter -> Descriptor_types.field_descriptor_proto_type -> unit 
(** [pp_field_descriptor_proto_type v] formats v *)

val pp_field_options_ctype : Format.formatter -> Descriptor_types.field_options_ctype -> unit 
(** [pp_field_options_ctype v] formats v *)

val pp_field_options_jstype : Format.formatter -> Descriptor_types.field_options_jstype -> unit 
(** [pp_field_options_jstype v] formats v *)

val pp_uninterpreted_option_name_part : Format.formatter -> Descriptor_types.uninterpreted_option_name_part -> unit 
(** [pp_uninterpreted_option_name_part v] formats v *)

val pp_uninterpreted_option : Format.formatter -> Descriptor_types.uninterpreted_option -> unit 
(** [pp_uninterpreted_option v] formats v *)

val pp_field_options : Format.formatter -> Descriptor_types.field_options -> unit 
(** [pp_field_options v] formats v *)

val pp_field_descriptor_proto : Format.formatter -> Descriptor_types.field_descriptor_proto -> unit 
(** [pp_field_descriptor_proto v] formats v *)

val pp_enum_value_options : Format.formatter -> Descriptor_types.enum_value_options -> unit 
(** [pp_enum_value_options v] formats v *)

val pp_enum_value_descriptor_proto : Format.formatter -> Descriptor_types.enum_value_descriptor_proto -> unit 
(** [pp_enum_value_descriptor_proto v] formats v *)

val pp_enum_options : Format.formatter -> Descriptor_types.enum_options -> unit 
(** [pp_enum_options v] formats v *)

val pp_enum_descriptor_proto_enum_reserved_range : Format.formatter -> Descriptor_types.enum_descriptor_proto_enum_reserved_range -> unit 
(** [pp_enum_descriptor_proto_enum_reserved_range v] formats v *)

val pp_enum_descriptor_proto : Format.formatter -> Descriptor_types.enum_descriptor_proto -> unit 
(** [pp_enum_descriptor_proto v] formats v *)

val pp_extension_range_options : Format.formatter -> Descriptor_types.extension_range_options -> unit 
(** [pp_extension_range_options v] formats v *)

val pp_descriptor_proto_extension_range : Format.formatter -> Descriptor_types.descriptor_proto_extension_range -> unit 
(** [pp_descriptor_proto_extension_range v] formats v *)

val pp_oneof_options : Format.formatter -> Descriptor_types.oneof_options -> unit 
(** [pp_oneof_options v] formats v *)

val pp_oneof_descriptor_proto : Format.formatter -> Descriptor_types.oneof_descriptor_proto -> unit 
(** [pp_oneof_descriptor_proto v] formats v *)

val pp_message_options : Format.formatter -> Descriptor_types.message_options -> unit 
(** [pp_message_options v] formats v *)

val pp_descriptor_proto_reserved_range : Format.formatter -> Descriptor_types.descriptor_proto_reserved_range -> unit 
(** [pp_descriptor_proto_reserved_range v] formats v *)

val pp_descriptor_proto : Format.formatter -> Descriptor_types.descriptor_proto -> unit 
(** [pp_descriptor_proto v] formats v *)

val pp_method_options_idempotency_level : Format.formatter -> Descriptor_types.method_options_idempotency_level -> unit 
(** [pp_method_options_idempotency_level v] formats v *)

val pp_method_options : Format.formatter -> Descriptor_types.method_options -> unit 
(** [pp_method_options v] formats v *)

val pp_method_descriptor_proto : Format.formatter -> Descriptor_types.method_descriptor_proto -> unit 
(** [pp_method_descriptor_proto v] formats v *)

val pp_service_options : Format.formatter -> Descriptor_types.service_options -> unit 
(** [pp_service_options v] formats v *)

val pp_service_descriptor_proto : Format.formatter -> Descriptor_types.service_descriptor_proto -> unit 
(** [pp_service_descriptor_proto v] formats v *)

val pp_file_options_optimize_mode : Format.formatter -> Descriptor_types.file_options_optimize_mode -> unit 
(** [pp_file_options_optimize_mode v] formats v *)

val pp_file_options : Format.formatter -> Descriptor_types.file_options -> unit 
(** [pp_file_options v] formats v *)

val pp_source_code_info_location : Format.formatter -> Descriptor_types.source_code_info_location -> unit 
(** [pp_source_code_info_location v] formats v *)

val pp_source_code_info : Format.formatter -> Descriptor_types.source_code_info -> unit 
(** [pp_source_code_info v] formats v *)

val pp_file_descriptor_proto : Format.formatter -> Descriptor_types.file_descriptor_proto -> unit 
(** [pp_file_descriptor_proto v] formats v *)

val pp_file_descriptor_set : Format.formatter -> Descriptor_types.file_descriptor_set -> unit 
(** [pp_file_descriptor_set v] formats v *)

val pp_generated_code_info_annotation : Format.formatter -> Descriptor_types.generated_code_info_annotation -> unit 
(** [pp_generated_code_info_annotation v] formats v *)

val pp_generated_code_info : Format.formatter -> Descriptor_types.generated_code_info -> unit 
(** [pp_generated_code_info v] formats v *)
