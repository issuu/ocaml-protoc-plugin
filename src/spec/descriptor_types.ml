[@@@ocaml.warning "-27-30-39"]


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

type extension_range_options = {
  uninterpreted_option : uninterpreted_option list;
}

type descriptor_proto_extension_range = {
  start : int option;
  end_ : int option;
  options : extension_range_options option;
}

type oneof_options = {
  uninterpreted_option : uninterpreted_option list;
}

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

type source_code_info = {
  location : source_code_info_location list;
}

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

type file_descriptor_set = {
  file : file_descriptor_proto list;
}

type generated_code_info_annotation = {
  path : int list;
  source_file : string option;
  begin_ : int option;
  end_ : int option;
}

type generated_code_info = {
  annotation : generated_code_info_annotation list;
}

let rec default_field_descriptor_proto_label () = (Label_optional:field_descriptor_proto_label)

let rec default_field_descriptor_proto_type () = (Type_double:field_descriptor_proto_type)

let rec default_field_options_ctype () = (String:field_options_ctype)

let rec default_field_options_jstype () = (Js_normal:field_options_jstype)

let rec default_uninterpreted_option_name_part 
  ?name_part:((name_part:string) = "")
  ?is_extension:((is_extension:bool) = false)
  () : uninterpreted_option_name_part  = {
  name_part;
  is_extension;
}

let rec default_uninterpreted_option 
  ?name:((name:uninterpreted_option_name_part list) = [])
  ?identifier_value:((identifier_value:string option) = None)
  ?positive_int_value:((positive_int_value:int option) = None)
  ?negative_int_value:((negative_int_value:int option) = None)
  ?double_value:((double_value:float option) = None)
  ?string_value:((string_value:bytes option) = None)
  ?aggregate_value:((aggregate_value:string option) = None)
  () : uninterpreted_option  = {
  name;
  identifier_value;
  positive_int_value;
  negative_int_value;
  double_value;
  string_value;
  aggregate_value;
}

let rec default_field_options 
  ?ctype:((ctype:field_options_ctype option) = Some (default_field_options_ctype ()))
  ?packed:((packed:bool option) = None)
  ?jstype:((jstype:field_options_jstype option) = Some (default_field_options_jstype ()))
  ?lazy_:((lazy_:bool option) = Some (false))
  ?deprecated:((deprecated:bool option) = Some (false))
  ?weak:((weak:bool option) = Some (false))
  ?uninterpreted_option:((uninterpreted_option:uninterpreted_option list) = [])
  () : field_options  = {
  ctype;
  packed;
  jstype;
  lazy_;
  deprecated;
  weak;
  uninterpreted_option;
}

let rec default_field_descriptor_proto 
  ?name:((name:string option) = None)
  ?number:((number:int option) = None)
  ?label:((label:field_descriptor_proto_label option) = None)
  ?type_:((type_:field_descriptor_proto_type option) = None)
  ?type_name:((type_name:string option) = None)
  ?extendee:((extendee:string option) = None)
  ?default_value:((default_value:string option) = None)
  ?oneof_index:((oneof_index:int option) = None)
  ?json_name:((json_name:string option) = None)
  ?options:((options:field_options option) = None)
  () : field_descriptor_proto  = {
  name;
  number;
  label;
  type_;
  type_name;
  extendee;
  default_value;
  oneof_index;
  json_name;
  options;
}

let rec default_enum_value_options 
  ?deprecated:((deprecated:bool option) = Some (false))
  ?uninterpreted_option:((uninterpreted_option:uninterpreted_option list) = [])
  () : enum_value_options  = {
  deprecated;
  uninterpreted_option;
}

let rec default_enum_value_descriptor_proto 
  ?name:((name:string option) = None)
  ?number:((number:int option) = None)
  ?options:((options:enum_value_options option) = None)
  () : enum_value_descriptor_proto  = {
  name;
  number;
  options;
}

let rec default_enum_options 
  ?allow_alias:((allow_alias:bool option) = None)
  ?deprecated:((deprecated:bool option) = Some (false))
  ?uninterpreted_option:((uninterpreted_option:uninterpreted_option list) = [])
  () : enum_options  = {
  allow_alias;
  deprecated;
  uninterpreted_option;
}

let rec default_enum_descriptor_proto_enum_reserved_range 
  ?start:((start:int option) = None)
  ?end_:((end_:int option) = None)
  () : enum_descriptor_proto_enum_reserved_range  = {
  start;
  end_;
}

let rec default_enum_descriptor_proto 
  ?name:((name:string option) = None)
  ?value:((value:enum_value_descriptor_proto list) = [])
  ?options:((options:enum_options option) = None)
  ?reserved_range:((reserved_range:enum_descriptor_proto_enum_reserved_range list) = [])
  ?reserved_name:((reserved_name:string list) = [])
  () : enum_descriptor_proto  = {
  name;
  value;
  options;
  reserved_range;
  reserved_name;
}

let rec default_extension_range_options 
  ?uninterpreted_option:((uninterpreted_option:uninterpreted_option list) = [])
  () : extension_range_options  = {
  uninterpreted_option;
}

let rec default_descriptor_proto_extension_range 
  ?start:((start:int option) = None)
  ?end_:((end_:int option) = None)
  ?options:((options:extension_range_options option) = None)
  () : descriptor_proto_extension_range  = {
  start;
  end_;
  options;
}

let rec default_oneof_options 
  ?uninterpreted_option:((uninterpreted_option:uninterpreted_option list) = [])
  () : oneof_options  = {
  uninterpreted_option;
}

let rec default_oneof_descriptor_proto 
  ?name:((name:string option) = None)
  ?options:((options:oneof_options option) = None)
  () : oneof_descriptor_proto  = {
  name;
  options;
}

let rec default_message_options 
  ?message_set_wire_format:((message_set_wire_format:bool option) = Some (false))
  ?no_standard_descriptor_accessor:((no_standard_descriptor_accessor:bool option) = Some (false))
  ?deprecated:((deprecated:bool option) = Some (false))
  ?map_entry:((map_entry:bool option) = None)
  ?uninterpreted_option:((uninterpreted_option:uninterpreted_option list) = [])
  () : message_options  = {
  message_set_wire_format;
  no_standard_descriptor_accessor;
  deprecated;
  map_entry;
  uninterpreted_option;
}

let rec default_descriptor_proto_reserved_range 
  ?start:((start:int option) = None)
  ?end_:((end_:int option) = None)
  () : descriptor_proto_reserved_range  = {
  start;
  end_;
}

let rec default_descriptor_proto 
  ?name:((name:string option) = None)
  ?field:((field:field_descriptor_proto list) = [])
  ?extension:((extension:field_descriptor_proto list) = [])
  ?nested_type:((nested_type:descriptor_proto list) = [])
  ?enum_type:((enum_type:enum_descriptor_proto list) = [])
  ?extension_range:((extension_range:descriptor_proto_extension_range list) = [])
  ?oneof_decl:((oneof_decl:oneof_descriptor_proto list) = [])
  ?options:((options:message_options option) = None)
  ?reserved_range:((reserved_range:descriptor_proto_reserved_range list) = [])
  ?reserved_name:((reserved_name:string list) = [])
  () : descriptor_proto  = {
  name;
  field;
  extension;
  nested_type;
  enum_type;
  extension_range;
  oneof_decl;
  options;
  reserved_range;
  reserved_name;
}

let rec default_method_options_idempotency_level () = (Idempotency_unknown:method_options_idempotency_level)

let rec default_method_options 
  ?deprecated:((deprecated:bool option) = Some (false))
  ?idempotency_level:((idempotency_level:method_options_idempotency_level option) = Some (default_method_options_idempotency_level ()))
  ?uninterpreted_option:((uninterpreted_option:uninterpreted_option list) = [])
  () : method_options  = {
  deprecated;
  idempotency_level;
  uninterpreted_option;
}

let rec default_method_descriptor_proto 
  ?name:((name:string option) = None)
  ?input_type:((input_type:string option) = None)
  ?output_type:((output_type:string option) = None)
  ?options:((options:method_options option) = None)
  ?client_streaming:((client_streaming:bool option) = Some (false))
  ?server_streaming:((server_streaming:bool option) = Some (false))
  () : method_descriptor_proto  = {
  name;
  input_type;
  output_type;
  options;
  client_streaming;
  server_streaming;
}

let rec default_service_options 
  ?deprecated:((deprecated:bool option) = Some (false))
  ?uninterpreted_option:((uninterpreted_option:uninterpreted_option list) = [])
  () : service_options  = {
  deprecated;
  uninterpreted_option;
}

let rec default_service_descriptor_proto 
  ?name:((name:string option) = None)
  ?method_:((method_:method_descriptor_proto list) = [])
  ?options:((options:service_options option) = None)
  () : service_descriptor_proto  = {
  name;
  method_;
  options;
}

let rec default_file_options_optimize_mode () = (Speed:file_options_optimize_mode)

let rec default_file_options 
  ?java_package:((java_package:string option) = None)
  ?java_outer_classname:((java_outer_classname:string option) = None)
  ?java_multiple_files:((java_multiple_files:bool option) = Some (false))
  ?java_generate_equals_and_hash:((java_generate_equals_and_hash:bool option) = None)
  ?java_string_check_utf8:((java_string_check_utf8:bool option) = Some (false))
  ?optimize_for:((optimize_for:file_options_optimize_mode option) = Some (default_file_options_optimize_mode ()))
  ?go_package:((go_package:string option) = None)
  ?cc_generic_services:((cc_generic_services:bool option) = Some (false))
  ?java_generic_services:((java_generic_services:bool option) = Some (false))
  ?py_generic_services:((py_generic_services:bool option) = Some (false))
  ?php_generic_services:((php_generic_services:bool option) = Some (false))
  ?deprecated:((deprecated:bool option) = Some (false))
  ?cc_enable_arenas:((cc_enable_arenas:bool option) = Some (false))
  ?objc_class_prefix:((objc_class_prefix:string option) = None)
  ?csharp_namespace:((csharp_namespace:string option) = None)
  ?swift_prefix:((swift_prefix:string option) = None)
  ?php_class_prefix:((php_class_prefix:string option) = None)
  ?php_namespace:((php_namespace:string option) = None)
  ?php_metadata_namespace:((php_metadata_namespace:string option) = None)
  ?ruby_package:((ruby_package:string option) = None)
  ?uninterpreted_option:((uninterpreted_option:uninterpreted_option list) = [])
  () : file_options  = {
  java_package;
  java_outer_classname;
  java_multiple_files;
  java_generate_equals_and_hash;
  java_string_check_utf8;
  optimize_for;
  go_package;
  cc_generic_services;
  java_generic_services;
  py_generic_services;
  php_generic_services;
  deprecated;
  cc_enable_arenas;
  objc_class_prefix;
  csharp_namespace;
  swift_prefix;
  php_class_prefix;
  php_namespace;
  php_metadata_namespace;
  ruby_package;
  uninterpreted_option;
}

let rec default_source_code_info_location 
  ?path:((path:int list) = [])
  ?span:((span:int list) = [])
  ?leading_comments:((leading_comments:string option) = None)
  ?trailing_comments:((trailing_comments:string option) = None)
  ?leading_detached_comments:((leading_detached_comments:string list) = [])
  () : source_code_info_location  = {
  path;
  span;
  leading_comments;
  trailing_comments;
  leading_detached_comments;
}

let rec default_source_code_info 
  ?location:((location:source_code_info_location list) = [])
  () : source_code_info  = {
  location;
}

let rec default_file_descriptor_proto 
  ?name:((name:string option) = None)
  ?package:((package:string option) = None)
  ?dependency:((dependency:string list) = [])
  ?public_dependency:((public_dependency:int list) = [])
  ?weak_dependency:((weak_dependency:int list) = [])
  ?message_type:((message_type:descriptor_proto list) = [])
  ?enum_type:((enum_type:enum_descriptor_proto list) = [])
  ?service:((service:service_descriptor_proto list) = [])
  ?extension:((extension:field_descriptor_proto list) = [])
  ?options:((options:file_options option) = None)
  ?source_code_info:((source_code_info:source_code_info option) = None)
  ?syntax:((syntax:string option) = None)
  () : file_descriptor_proto  = {
  name;
  package;
  dependency;
  public_dependency;
  weak_dependency;
  message_type;
  enum_type;
  service;
  extension;
  options;
  source_code_info;
  syntax;
}

let rec default_file_descriptor_set 
  ?file:((file:file_descriptor_proto list) = [])
  () : file_descriptor_set  = {
  file;
}

let rec default_generated_code_info_annotation 
  ?path:((path:int list) = [])
  ?source_file:((source_file:string option) = None)
  ?begin_:((begin_:int option) = None)
  ?end_:((end_:int option) = None)
  () : generated_code_info_annotation  = {
  path;
  source_file;
  begin_;
  end_;
}

let rec default_generated_code_info 
  ?annotation:((annotation:generated_code_info_annotation list) = [])
  () : generated_code_info  = {
  annotation;
}
