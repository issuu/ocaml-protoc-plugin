(** descriptor.proto Types *)

(** {2 Types} *)

type field_descriptor_proto_label =
  | Label_optional
  | Label_required
  | Label_repeated

type field_descriptor_proto_type =
  | Type_double
  | Type_float
  | Type_int64
  | Type_uint64
  | Type_int32
  | Type_fixed64
  | Type_fixed32
  | Type_bool
  | Type_string
  | Type_group
  | Type_message
  | Type_bytes
  | Type_uint32
  | Type_enum
  | Type_sfixed32
  | Type_sfixed64
  | Type_sint32
  | Type_sint64

type field_options_ctype =
  | String
  | Cord
  | String_piece

type field_options_jstype =
  | Js_normal
  | Js_string
  | Js_number

type uninterpreted_option_name_part = {
  name_part : string;
  is_extension : bool;
}

type uninterpreted_option = {
  name : uninterpreted_option_name_part list;
  identifier_value : string option;
  positive_int_value : int option;
  negative_int_value : int option;
  double_value : float option;
  string_value : bytes option;
  aggregate_value : string option;
}

type field_options = {
  ctype : field_options_ctype option;
  packed : bool option;
  jstype : field_options_jstype option;
  lazy_ : bool option;
  deprecated : bool option;
  weak : bool option;
  uninterpreted_option : uninterpreted_option list;
}

type field_descriptor_proto = {
  name : string option;
  number : int option;
  label : field_descriptor_proto_label option;
  type_ : field_descriptor_proto_type option;
  type_name : string option;
  extendee : string option;
  default_value : string option;
  oneof_index : int option;
  json_name : string option;
  options : field_options option;
}

type enum_value_options = {
  deprecated : bool option;
  uninterpreted_option : uninterpreted_option list;
}

type enum_value_descriptor_proto = {
  name : string option;
  number : int option;
  options : enum_value_options option;
}

type enum_options = {
  allow_alias : bool option;
  deprecated : bool option;
  uninterpreted_option : uninterpreted_option list;
}

type enum_descriptor_proto_enum_reserved_range = {
  start : int option;
  end_ : int option;
}

type enum_descriptor_proto = {
  name : string option;
  value : enum_value_descriptor_proto list;
  options : enum_options option;
  reserved_range : enum_descriptor_proto_enum_reserved_range list;
  reserved_name : string list;
}

type extension_range_options = {uninterpreted_option : uninterpreted_option list}

type descriptor_proto_extension_range = {
  start : int option;
  end_ : int option;
  options : extension_range_options option;
}

type oneof_options = {uninterpreted_option : uninterpreted_option list}

type oneof_descriptor_proto = {
  name : string option;
  options : oneof_options option;
}

type message_options = {
  message_set_wire_format : bool option;
  no_standard_descriptor_accessor : bool option;
  deprecated : bool option;
  map_entry : bool option;
  uninterpreted_option : uninterpreted_option list;
}

type descriptor_proto_reserved_range = {
  start : int option;
  end_ : int option;
}

type descriptor_proto = {
  name : string option;
  field : field_descriptor_proto list;
  extension : field_descriptor_proto list;
  nested_type : descriptor_proto list;
  enum_type : enum_descriptor_proto list;
  extension_range : descriptor_proto_extension_range list;
  oneof_decl : oneof_descriptor_proto list;
  options : message_options option;
  reserved_range : descriptor_proto_reserved_range list;
  reserved_name : string list;
}

type method_options_idempotency_level =
  | Idempotency_unknown
  | No_side_effects
  | Idempotent

type method_options = {
  deprecated : bool option;
  idempotency_level : method_options_idempotency_level option;
  uninterpreted_option : uninterpreted_option list;
}

type method_descriptor_proto = {
  name : string option;
  input_type : string option;
  output_type : string option;
  options : method_options option;
  client_streaming : bool option;
  server_streaming : bool option;
}

type service_options = {
  deprecated : bool option;
  uninterpreted_option : uninterpreted_option list;
}

type service_descriptor_proto = {
  name : string option;
  method_ : method_descriptor_proto list;
  options : service_options option;
}

type file_options_optimize_mode =
  | Speed
  | Code_size
  | Lite_runtime

type file_options = {
  java_package : string option;
  java_outer_classname : string option;
  java_multiple_files : bool option;
  java_generate_equals_and_hash : bool option;
  java_string_check_utf8 : bool option;
  optimize_for : file_options_optimize_mode option;
  go_package : string option;
  cc_generic_services : bool option;
  java_generic_services : bool option;
  py_generic_services : bool option;
  php_generic_services : bool option;
  deprecated : bool option;
  cc_enable_arenas : bool option;
  objc_class_prefix : string option;
  csharp_namespace : string option;
  swift_prefix : string option;
  php_class_prefix : string option;
  php_namespace : string option;
  php_metadata_namespace : string option;
  ruby_package : string option;
  uninterpreted_option : uninterpreted_option list;
}

type source_code_info_location = {
  path : int list;
  span : int list;
  leading_comments : string option;
  trailing_comments : string option;
  leading_detached_comments : string list;
}

type source_code_info = {location : source_code_info_location list}

type file_descriptor_proto = {
  name : string option;
  package : string option;
  dependency : string list;
  public_dependency : int list;
  weak_dependency : int list;
  message_type : descriptor_proto list;
  enum_type : enum_descriptor_proto list;
  service : service_descriptor_proto list;
  extension : field_descriptor_proto list;
  options : file_options option;
  source_code_info : source_code_info option;
  syntax : string option;
}

type file_descriptor_set = {file : file_descriptor_proto list}

type generated_code_info_annotation = {
  path : int list;
  source_file : string option;
  begin_ : int option;
  end_ : int option;
}

type generated_code_info = {annotation : generated_code_info_annotation list}

(** {2 Default values} *)

(** [default_field_descriptor_proto_label ()] is the default value for type [field_descriptor_proto_label] *)
val default_field_descriptor_proto_label : unit -> field_descriptor_proto_label

(** [default_field_descriptor_proto_type ()] is the default value for type [field_descriptor_proto_type] *)
val default_field_descriptor_proto_type : unit -> field_descriptor_proto_type

(** [default_field_options_ctype ()] is the default value for type [field_options_ctype] *)
val default_field_options_ctype : unit -> field_options_ctype

(** [default_field_options_jstype ()] is the default value for type [field_options_jstype] *)
val default_field_options_jstype : unit -> field_options_jstype

(** [default_uninterpreted_option_name_part ()] is the default value for type [uninterpreted_option_name_part] *)
val default_uninterpreted_option_name_part
  :  ?name_part:string ->
  ?is_extension:bool ->
  unit ->
  uninterpreted_option_name_part

(** [default_uninterpreted_option ()] is the default value for type [uninterpreted_option] *)
val default_uninterpreted_option
  :  ?name:uninterpreted_option_name_part list ->
  ?identifier_value:string option ->
  ?positive_int_value:int option ->
  ?negative_int_value:int option ->
  ?double_value:float option ->
  ?string_value:bytes option ->
  ?aggregate_value:string option ->
  unit ->
  uninterpreted_option

(** [default_field_options ()] is the default value for type [field_options] *)
val default_field_options
  :  ?ctype:field_options_ctype option ->
  ?packed:bool option ->
  ?jstype:field_options_jstype option ->
  ?lazy_:bool option ->
  ?deprecated:bool option ->
  ?weak:bool option ->
  ?uninterpreted_option:uninterpreted_option list ->
  unit ->
  field_options

(** [default_field_descriptor_proto ()] is the default value for type [field_descriptor_proto] *)
val default_field_descriptor_proto
  :  ?name:string option ->
  ?number:int option ->
  ?label:field_descriptor_proto_label option ->
  ?type_:field_descriptor_proto_type option ->
  ?type_name:string option ->
  ?extendee:string option ->
  ?default_value:string option ->
  ?oneof_index:int option ->
  ?json_name:string option ->
  ?options:field_options option ->
  unit ->
  field_descriptor_proto

(** [default_enum_value_options ()] is the default value for type [enum_value_options] *)
val default_enum_value_options
  :  ?deprecated:bool option ->
  ?uninterpreted_option:uninterpreted_option list ->
  unit ->
  enum_value_options

(** [default_enum_value_descriptor_proto ()] is the default value for type [enum_value_descriptor_proto] *)
val default_enum_value_descriptor_proto
  :  ?name:string option ->
  ?number:int option ->
  ?options:enum_value_options option ->
  unit ->
  enum_value_descriptor_proto

(** [default_enum_options ()] is the default value for type [enum_options] *)
val default_enum_options
  :  ?allow_alias:bool option ->
  ?deprecated:bool option ->
  ?uninterpreted_option:uninterpreted_option list ->
  unit ->
  enum_options

(** [default_enum_descriptor_proto_enum_reserved_range ()] is the default value for type [enum_descriptor_proto_enum_reserved_range] *)
val default_enum_descriptor_proto_enum_reserved_range
  :  ?start:int option ->
  ?end_:int option ->
  unit ->
  enum_descriptor_proto_enum_reserved_range

(** [default_enum_descriptor_proto ()] is the default value for type [enum_descriptor_proto] *)
val default_enum_descriptor_proto
  :  ?name:string option ->
  ?value:enum_value_descriptor_proto list ->
  ?options:enum_options option ->
  ?reserved_range:enum_descriptor_proto_enum_reserved_range list ->
  ?reserved_name:string list ->
  unit ->
  enum_descriptor_proto

(** [default_extension_range_options ()] is the default value for type [extension_range_options] *)
val default_extension_range_options
  :  ?uninterpreted_option:uninterpreted_option list ->
  unit ->
  extension_range_options

(** [default_descriptor_proto_extension_range ()] is the default value for type [descriptor_proto_extension_range] *)
val default_descriptor_proto_extension_range
  :  ?start:int option ->
  ?end_:int option ->
  ?options:extension_range_options option ->
  unit ->
  descriptor_proto_extension_range

(** [default_oneof_options ()] is the default value for type [oneof_options] *)
val default_oneof_options
  :  ?uninterpreted_option:uninterpreted_option list ->
  unit ->
  oneof_options

(** [default_oneof_descriptor_proto ()] is the default value for type [oneof_descriptor_proto] *)
val default_oneof_descriptor_proto
  :  ?name:string option ->
  ?options:oneof_options option ->
  unit ->
  oneof_descriptor_proto

(** [default_message_options ()] is the default value for type [message_options] *)
val default_message_options
  :  ?message_set_wire_format:bool option ->
  ?no_standard_descriptor_accessor:bool option ->
  ?deprecated:bool option ->
  ?map_entry:bool option ->
  ?uninterpreted_option:uninterpreted_option list ->
  unit ->
  message_options

(** [default_descriptor_proto_reserved_range ()] is the default value for type [descriptor_proto_reserved_range] *)
val default_descriptor_proto_reserved_range
  :  ?start:int option ->
  ?end_:int option ->
  unit ->
  descriptor_proto_reserved_range

(** [default_descriptor_proto ()] is the default value for type [descriptor_proto] *)
val default_descriptor_proto
  :  ?name:string option ->
  ?field:field_descriptor_proto list ->
  ?extension:field_descriptor_proto list ->
  ?nested_type:descriptor_proto list ->
  ?enum_type:enum_descriptor_proto list ->
  ?extension_range:descriptor_proto_extension_range list ->
  ?oneof_decl:oneof_descriptor_proto list ->
  ?options:message_options option ->
  ?reserved_range:descriptor_proto_reserved_range list ->
  ?reserved_name:string list ->
  unit ->
  descriptor_proto

(** [default_method_options_idempotency_level ()] is the default value for type [method_options_idempotency_level] *)
val default_method_options_idempotency_level : unit -> method_options_idempotency_level

(** [default_method_options ()] is the default value for type [method_options] *)
val default_method_options
  :  ?deprecated:bool option ->
  ?idempotency_level:method_options_idempotency_level option ->
  ?uninterpreted_option:uninterpreted_option list ->
  unit ->
  method_options

(** [default_method_descriptor_proto ()] is the default value for type [method_descriptor_proto] *)
val default_method_descriptor_proto
  :  ?name:string option ->
  ?input_type:string option ->
  ?output_type:string option ->
  ?options:method_options option ->
  ?client_streaming:bool option ->
  ?server_streaming:bool option ->
  unit ->
  method_descriptor_proto

(** [default_service_options ()] is the default value for type [service_options] *)
val default_service_options
  :  ?deprecated:bool option ->
  ?uninterpreted_option:uninterpreted_option list ->
  unit ->
  service_options

(** [default_service_descriptor_proto ()] is the default value for type [service_descriptor_proto] *)
val default_service_descriptor_proto
  :  ?name:string option ->
  ?method_:method_descriptor_proto list ->
  ?options:service_options option ->
  unit ->
  service_descriptor_proto

(** [default_file_options_optimize_mode ()] is the default value for type [file_options_optimize_mode] *)
val default_file_options_optimize_mode : unit -> file_options_optimize_mode

(** [default_file_options ()] is the default value for type [file_options] *)
val default_file_options
  :  ?java_package:string option ->
  ?java_outer_classname:string option ->
  ?java_multiple_files:bool option ->
  ?java_generate_equals_and_hash:bool option ->
  ?java_string_check_utf8:bool option ->
  ?optimize_for:file_options_optimize_mode option ->
  ?go_package:string option ->
  ?cc_generic_services:bool option ->
  ?java_generic_services:bool option ->
  ?py_generic_services:bool option ->
  ?php_generic_services:bool option ->
  ?deprecated:bool option ->
  ?cc_enable_arenas:bool option ->
  ?objc_class_prefix:string option ->
  ?csharp_namespace:string option ->
  ?swift_prefix:string option ->
  ?php_class_prefix:string option ->
  ?php_namespace:string option ->
  ?php_metadata_namespace:string option ->
  ?ruby_package:string option ->
  ?uninterpreted_option:uninterpreted_option list ->
  unit ->
  file_options

(** [default_source_code_info_location ()] is the default value for type [source_code_info_location] *)
val default_source_code_info_location
  :  ?path:int list ->
  ?span:int list ->
  ?leading_comments:string option ->
  ?trailing_comments:string option ->
  ?leading_detached_comments:string list ->
  unit ->
  source_code_info_location

(** [default_source_code_info ()] is the default value for type [source_code_info] *)
val default_source_code_info
  :  ?location:source_code_info_location list ->
  unit ->
  source_code_info

(** [default_file_descriptor_proto ()] is the default value for type [file_descriptor_proto] *)
val default_file_descriptor_proto
  :  ?name:string option ->
  ?package:string option ->
  ?dependency:string list ->
  ?public_dependency:int list ->
  ?weak_dependency:int list ->
  ?message_type:descriptor_proto list ->
  ?enum_type:enum_descriptor_proto list ->
  ?service:service_descriptor_proto list ->
  ?extension:field_descriptor_proto list ->
  ?options:file_options option ->
  ?source_code_info:source_code_info option ->
  ?syntax:string option ->
  unit ->
  file_descriptor_proto

(** [default_file_descriptor_set ()] is the default value for type [file_descriptor_set] *)
val default_file_descriptor_set
  :  ?file:file_descriptor_proto list ->
  unit ->
  file_descriptor_set

(** [default_generated_code_info_annotation ()] is the default value for type [generated_code_info_annotation] *)
val default_generated_code_info_annotation
  :  ?path:int list ->
  ?source_file:string option ->
  ?begin_:int option ->
  ?end_:int option ->
  unit ->
  generated_code_info_annotation

(** [default_generated_code_info ()] is the default value for type [generated_code_info] *)
val default_generated_code_info
  :  ?annotation:generated_code_info_annotation list ->
  unit ->
  generated_code_info
