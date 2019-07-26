[@@@ocaml.warning "-27-30-39"]

let rec pp_field_descriptor_proto_label fmt
    (v : Descriptor_types.field_descriptor_proto_label)
  =
  match v with
  | Descriptor_types.Label_optional -> Format.fprintf fmt "Label_optional"
  | Descriptor_types.Label_required -> Format.fprintf fmt "Label_required"
  | Descriptor_types.Label_repeated -> Format.fprintf fmt "Label_repeated"

let rec pp_field_descriptor_proto_type fmt
    (v : Descriptor_types.field_descriptor_proto_type)
  =
  match v with
  | Descriptor_types.Type_double -> Format.fprintf fmt "Type_double"
  | Descriptor_types.Type_float -> Format.fprintf fmt "Type_float"
  | Descriptor_types.Type_int64 -> Format.fprintf fmt "Type_int64"
  | Descriptor_types.Type_uint64 -> Format.fprintf fmt "Type_uint64"
  | Descriptor_types.Type_int32 -> Format.fprintf fmt "Type_int32"
  | Descriptor_types.Type_fixed64 -> Format.fprintf fmt "Type_fixed64"
  | Descriptor_types.Type_fixed32 -> Format.fprintf fmt "Type_fixed32"
  | Descriptor_types.Type_bool -> Format.fprintf fmt "Type_bool"
  | Descriptor_types.Type_string -> Format.fprintf fmt "Type_string"
  | Descriptor_types.Type_group -> Format.fprintf fmt "Type_group"
  | Descriptor_types.Type_message -> Format.fprintf fmt "Type_message"
  | Descriptor_types.Type_bytes -> Format.fprintf fmt "Type_bytes"
  | Descriptor_types.Type_uint32 -> Format.fprintf fmt "Type_uint32"
  | Descriptor_types.Type_enum -> Format.fprintf fmt "Type_enum"
  | Descriptor_types.Type_sfixed32 -> Format.fprintf fmt "Type_sfixed32"
  | Descriptor_types.Type_sfixed64 -> Format.fprintf fmt "Type_sfixed64"
  | Descriptor_types.Type_sint32 -> Format.fprintf fmt "Type_sint32"
  | Descriptor_types.Type_sint64 -> Format.fprintf fmt "Type_sint64"

let rec pp_field_options_ctype fmt (v : Descriptor_types.field_options_ctype) =
  match v with
  | Descriptor_types.String -> Format.fprintf fmt "String"
  | Descriptor_types.Cord -> Format.fprintf fmt "Cord"
  | Descriptor_types.String_piece -> Format.fprintf fmt "String_piece"

let rec pp_field_options_jstype fmt (v : Descriptor_types.field_options_jstype) =
  match v with
  | Descriptor_types.Js_normal -> Format.fprintf fmt "Js_normal"
  | Descriptor_types.Js_string -> Format.fprintf fmt "Js_string"
  | Descriptor_types.Js_number -> Format.fprintf fmt "Js_number"

let rec pp_uninterpreted_option_name_part fmt
    (v : Descriptor_types.uninterpreted_option_name_part)
  =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "name_part"
      Pbrt.Pp.pp_string
      fmt
      v.Descriptor_types.name_part;
    Pbrt.Pp.pp_record_field
      "is_extension"
      Pbrt.Pp.pp_bool
      fmt
      v.Descriptor_types.is_extension;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_uninterpreted_option fmt (v : Descriptor_types.uninterpreted_option) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "name"
      (Pbrt.Pp.pp_list pp_uninterpreted_option_name_part)
      fmt
      v.Descriptor_types.name;
    Pbrt.Pp.pp_record_field
      "identifier_value"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.identifier_value;
    Pbrt.Pp.pp_record_field
      "positive_int_value"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_int)
      fmt
      v.Descriptor_types.positive_int_value;
    Pbrt.Pp.pp_record_field
      "negative_int_value"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_int)
      fmt
      v.Descriptor_types.negative_int_value;
    Pbrt.Pp.pp_record_field
      "double_value"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_float)
      fmt
      v.Descriptor_types.double_value;
    Pbrt.Pp.pp_record_field
      "string_value"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bytes)
      fmt
      v.Descriptor_types.string_value;
    Pbrt.Pp.pp_record_field
      "aggregate_value"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.aggregate_value;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_field_options fmt (v : Descriptor_types.field_options) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "ctype"
      (Pbrt.Pp.pp_option pp_field_options_ctype)
      fmt
      v.Descriptor_types.ctype;
    Pbrt.Pp.pp_record_field
      "packed"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.packed;
    Pbrt.Pp.pp_record_field
      "jstype"
      (Pbrt.Pp.pp_option pp_field_options_jstype)
      fmt
      v.Descriptor_types.jstype;
    Pbrt.Pp.pp_record_field
      "lazy_"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.lazy_;
    Pbrt.Pp.pp_record_field
      "deprecated"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.deprecated;
    Pbrt.Pp.pp_record_field
      "weak"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.weak;
    Pbrt.Pp.pp_record_field
      "uninterpreted_option"
      (Pbrt.Pp.pp_list pp_uninterpreted_option)
      fmt
      v.Descriptor_types.uninterpreted_option;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_field_descriptor_proto fmt (v : Descriptor_types.field_descriptor_proto) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "name"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.name;
    Pbrt.Pp.pp_record_field
      "number"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_int)
      fmt
      v.Descriptor_types.number;
    Pbrt.Pp.pp_record_field
      "label"
      (Pbrt.Pp.pp_option pp_field_descriptor_proto_label)
      fmt
      v.Descriptor_types.label;
    Pbrt.Pp.pp_record_field
      "type_"
      (Pbrt.Pp.pp_option pp_field_descriptor_proto_type)
      fmt
      v.Descriptor_types.type_;
    Pbrt.Pp.pp_record_field
      "type_name"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.type_name;
    Pbrt.Pp.pp_record_field
      "extendee"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.extendee;
    Pbrt.Pp.pp_record_field
      "default_value"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.default_value;
    Pbrt.Pp.pp_record_field
      "oneof_index"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_int)
      fmt
      v.Descriptor_types.oneof_index;
    Pbrt.Pp.pp_record_field
      "json_name"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.json_name;
    Pbrt.Pp.pp_record_field
      "options"
      (Pbrt.Pp.pp_option pp_field_options)
      fmt
      v.Descriptor_types.options;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_enum_value_options fmt (v : Descriptor_types.enum_value_options) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "deprecated"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.deprecated;
    Pbrt.Pp.pp_record_field
      "uninterpreted_option"
      (Pbrt.Pp.pp_list pp_uninterpreted_option)
      fmt
      v.Descriptor_types.uninterpreted_option;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_enum_value_descriptor_proto fmt
    (v : Descriptor_types.enum_value_descriptor_proto)
  =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "name"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.name;
    Pbrt.Pp.pp_record_field
      "number"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_int)
      fmt
      v.Descriptor_types.number;
    Pbrt.Pp.pp_record_field
      "options"
      (Pbrt.Pp.pp_option pp_enum_value_options)
      fmt
      v.Descriptor_types.options;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_enum_options fmt (v : Descriptor_types.enum_options) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "allow_alias"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.allow_alias;
    Pbrt.Pp.pp_record_field
      "deprecated"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.deprecated;
    Pbrt.Pp.pp_record_field
      "uninterpreted_option"
      (Pbrt.Pp.pp_list pp_uninterpreted_option)
      fmt
      v.Descriptor_types.uninterpreted_option;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_enum_descriptor_proto_enum_reserved_range fmt
    (v : Descriptor_types.enum_descriptor_proto_enum_reserved_range)
  =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "start"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_int)
      fmt
      v.Descriptor_types.start;
    Pbrt.Pp.pp_record_field
      "end_"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_int)
      fmt
      v.Descriptor_types.end_;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_enum_descriptor_proto fmt (v : Descriptor_types.enum_descriptor_proto) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "name"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.name;
    Pbrt.Pp.pp_record_field
      "value"
      (Pbrt.Pp.pp_list pp_enum_value_descriptor_proto)
      fmt
      v.Descriptor_types.value;
    Pbrt.Pp.pp_record_field
      "options"
      (Pbrt.Pp.pp_option pp_enum_options)
      fmt
      v.Descriptor_types.options;
    Pbrt.Pp.pp_record_field
      "reserved_range"
      (Pbrt.Pp.pp_list pp_enum_descriptor_proto_enum_reserved_range)
      fmt
      v.Descriptor_types.reserved_range;
    Pbrt.Pp.pp_record_field
      "reserved_name"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.reserved_name;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_extension_range_options fmt (v : Descriptor_types.extension_range_options) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "uninterpreted_option"
      (Pbrt.Pp.pp_list pp_uninterpreted_option)
      fmt
      v.Descriptor_types.uninterpreted_option;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_descriptor_proto_extension_range fmt
    (v : Descriptor_types.descriptor_proto_extension_range)
  =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "start"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_int)
      fmt
      v.Descriptor_types.start;
    Pbrt.Pp.pp_record_field
      "end_"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_int)
      fmt
      v.Descriptor_types.end_;
    Pbrt.Pp.pp_record_field
      "options"
      (Pbrt.Pp.pp_option pp_extension_range_options)
      fmt
      v.Descriptor_types.options;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_oneof_options fmt (v : Descriptor_types.oneof_options) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "uninterpreted_option"
      (Pbrt.Pp.pp_list pp_uninterpreted_option)
      fmt
      v.Descriptor_types.uninterpreted_option;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_oneof_descriptor_proto fmt (v : Descriptor_types.oneof_descriptor_proto) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "name"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.name;
    Pbrt.Pp.pp_record_field
      "options"
      (Pbrt.Pp.pp_option pp_oneof_options)
      fmt
      v.Descriptor_types.options;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_message_options fmt (v : Descriptor_types.message_options) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "message_set_wire_format"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.message_set_wire_format;
    Pbrt.Pp.pp_record_field
      "no_standard_descriptor_accessor"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.no_standard_descriptor_accessor;
    Pbrt.Pp.pp_record_field
      "deprecated"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.deprecated;
    Pbrt.Pp.pp_record_field
      "map_entry"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.map_entry;
    Pbrt.Pp.pp_record_field
      "uninterpreted_option"
      (Pbrt.Pp.pp_list pp_uninterpreted_option)
      fmt
      v.Descriptor_types.uninterpreted_option;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_descriptor_proto_reserved_range fmt
    (v : Descriptor_types.descriptor_proto_reserved_range)
  =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "start"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_int)
      fmt
      v.Descriptor_types.start;
    Pbrt.Pp.pp_record_field
      "end_"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_int)
      fmt
      v.Descriptor_types.end_;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_descriptor_proto fmt (v : Descriptor_types.descriptor_proto) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "name"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.name;
    Pbrt.Pp.pp_record_field
      "field"
      (Pbrt.Pp.pp_list pp_field_descriptor_proto)
      fmt
      v.Descriptor_types.field;
    Pbrt.Pp.pp_record_field
      "extension"
      (Pbrt.Pp.pp_list pp_field_descriptor_proto)
      fmt
      v.Descriptor_types.extension;
    Pbrt.Pp.pp_record_field
      "nested_type"
      (Pbrt.Pp.pp_list pp_descriptor_proto)
      fmt
      v.Descriptor_types.nested_type;
    Pbrt.Pp.pp_record_field
      "enum_type"
      (Pbrt.Pp.pp_list pp_enum_descriptor_proto)
      fmt
      v.Descriptor_types.enum_type;
    Pbrt.Pp.pp_record_field
      "extension_range"
      (Pbrt.Pp.pp_list pp_descriptor_proto_extension_range)
      fmt
      v.Descriptor_types.extension_range;
    Pbrt.Pp.pp_record_field
      "oneof_decl"
      (Pbrt.Pp.pp_list pp_oneof_descriptor_proto)
      fmt
      v.Descriptor_types.oneof_decl;
    Pbrt.Pp.pp_record_field
      "options"
      (Pbrt.Pp.pp_option pp_message_options)
      fmt
      v.Descriptor_types.options;
    Pbrt.Pp.pp_record_field
      "reserved_range"
      (Pbrt.Pp.pp_list pp_descriptor_proto_reserved_range)
      fmt
      v.Descriptor_types.reserved_range;
    Pbrt.Pp.pp_record_field
      "reserved_name"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.reserved_name;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_method_options_idempotency_level fmt
    (v : Descriptor_types.method_options_idempotency_level)
  =
  match v with
  | Descriptor_types.Idempotency_unknown -> Format.fprintf fmt "Idempotency_unknown"
  | Descriptor_types.No_side_effects -> Format.fprintf fmt "No_side_effects"
  | Descriptor_types.Idempotent -> Format.fprintf fmt "Idempotent"

let rec pp_method_options fmt (v : Descriptor_types.method_options) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "deprecated"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.deprecated;
    Pbrt.Pp.pp_record_field
      "idempotency_level"
      (Pbrt.Pp.pp_option pp_method_options_idempotency_level)
      fmt
      v.Descriptor_types.idempotency_level;
    Pbrt.Pp.pp_record_field
      "uninterpreted_option"
      (Pbrt.Pp.pp_list pp_uninterpreted_option)
      fmt
      v.Descriptor_types.uninterpreted_option;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_method_descriptor_proto fmt (v : Descriptor_types.method_descriptor_proto) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "name"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.name;
    Pbrt.Pp.pp_record_field
      "input_type"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.input_type;
    Pbrt.Pp.pp_record_field
      "output_type"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.output_type;
    Pbrt.Pp.pp_record_field
      "options"
      (Pbrt.Pp.pp_option pp_method_options)
      fmt
      v.Descriptor_types.options;
    Pbrt.Pp.pp_record_field
      "client_streaming"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.client_streaming;
    Pbrt.Pp.pp_record_field
      "server_streaming"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.server_streaming;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_service_options fmt (v : Descriptor_types.service_options) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "deprecated"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.deprecated;
    Pbrt.Pp.pp_record_field
      "uninterpreted_option"
      (Pbrt.Pp.pp_list pp_uninterpreted_option)
      fmt
      v.Descriptor_types.uninterpreted_option;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_service_descriptor_proto fmt (v : Descriptor_types.service_descriptor_proto) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "name"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.name;
    Pbrt.Pp.pp_record_field
      "method_"
      (Pbrt.Pp.pp_list pp_method_descriptor_proto)
      fmt
      v.Descriptor_types.method_;
    Pbrt.Pp.pp_record_field
      "options"
      (Pbrt.Pp.pp_option pp_service_options)
      fmt
      v.Descriptor_types.options;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_file_options_optimize_mode fmt
    (v : Descriptor_types.file_options_optimize_mode)
  =
  match v with
  | Descriptor_types.Speed -> Format.fprintf fmt "Speed"
  | Descriptor_types.Code_size -> Format.fprintf fmt "Code_size"
  | Descriptor_types.Lite_runtime -> Format.fprintf fmt "Lite_runtime"

let rec pp_file_options fmt (v : Descriptor_types.file_options) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "java_package"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.java_package;
    Pbrt.Pp.pp_record_field
      "java_outer_classname"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.java_outer_classname;
    Pbrt.Pp.pp_record_field
      "java_multiple_files"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.java_multiple_files;
    Pbrt.Pp.pp_record_field
      "java_generate_equals_and_hash"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.java_generate_equals_and_hash;
    Pbrt.Pp.pp_record_field
      "java_string_check_utf8"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.java_string_check_utf8;
    Pbrt.Pp.pp_record_field
      "optimize_for"
      (Pbrt.Pp.pp_option pp_file_options_optimize_mode)
      fmt
      v.Descriptor_types.optimize_for;
    Pbrt.Pp.pp_record_field
      "go_package"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.go_package;
    Pbrt.Pp.pp_record_field
      "cc_generic_services"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.cc_generic_services;
    Pbrt.Pp.pp_record_field
      "java_generic_services"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.java_generic_services;
    Pbrt.Pp.pp_record_field
      "py_generic_services"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.py_generic_services;
    Pbrt.Pp.pp_record_field
      "php_generic_services"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.php_generic_services;
    Pbrt.Pp.pp_record_field
      "deprecated"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.deprecated;
    Pbrt.Pp.pp_record_field
      "cc_enable_arenas"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bool)
      fmt
      v.Descriptor_types.cc_enable_arenas;
    Pbrt.Pp.pp_record_field
      "objc_class_prefix"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.objc_class_prefix;
    Pbrt.Pp.pp_record_field
      "csharp_namespace"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.csharp_namespace;
    Pbrt.Pp.pp_record_field
      "swift_prefix"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.swift_prefix;
    Pbrt.Pp.pp_record_field
      "php_class_prefix"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.php_class_prefix;
    Pbrt.Pp.pp_record_field
      "php_namespace"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.php_namespace;
    Pbrt.Pp.pp_record_field
      "php_metadata_namespace"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.php_metadata_namespace;
    Pbrt.Pp.pp_record_field
      "ruby_package"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.ruby_package;
    Pbrt.Pp.pp_record_field
      "uninterpreted_option"
      (Pbrt.Pp.pp_list pp_uninterpreted_option)
      fmt
      v.Descriptor_types.uninterpreted_option;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_source_code_info_location fmt (v : Descriptor_types.source_code_info_location)
  =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "path"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_int)
      fmt
      v.Descriptor_types.path;
    Pbrt.Pp.pp_record_field
      "span"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_int)
      fmt
      v.Descriptor_types.span;
    Pbrt.Pp.pp_record_field
      "leading_comments"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.leading_comments;
    Pbrt.Pp.pp_record_field
      "trailing_comments"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.trailing_comments;
    Pbrt.Pp.pp_record_field
      "leading_detached_comments"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.leading_detached_comments;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_source_code_info fmt (v : Descriptor_types.source_code_info) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "location"
      (Pbrt.Pp.pp_list pp_source_code_info_location)
      fmt
      v.Descriptor_types.location;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_file_descriptor_proto fmt (v : Descriptor_types.file_descriptor_proto) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "name"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.name;
    Pbrt.Pp.pp_record_field
      "package"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.package;
    Pbrt.Pp.pp_record_field
      "dependency"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.dependency;
    Pbrt.Pp.pp_record_field
      "public_dependency"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_int)
      fmt
      v.Descriptor_types.public_dependency;
    Pbrt.Pp.pp_record_field
      "weak_dependency"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_int)
      fmt
      v.Descriptor_types.weak_dependency;
    Pbrt.Pp.pp_record_field
      "message_type"
      (Pbrt.Pp.pp_list pp_descriptor_proto)
      fmt
      v.Descriptor_types.message_type;
    Pbrt.Pp.pp_record_field
      "enum_type"
      (Pbrt.Pp.pp_list pp_enum_descriptor_proto)
      fmt
      v.Descriptor_types.enum_type;
    Pbrt.Pp.pp_record_field
      "service"
      (Pbrt.Pp.pp_list pp_service_descriptor_proto)
      fmt
      v.Descriptor_types.service;
    Pbrt.Pp.pp_record_field
      "extension"
      (Pbrt.Pp.pp_list pp_field_descriptor_proto)
      fmt
      v.Descriptor_types.extension;
    Pbrt.Pp.pp_record_field
      "options"
      (Pbrt.Pp.pp_option pp_file_options)
      fmt
      v.Descriptor_types.options;
    Pbrt.Pp.pp_record_field
      "source_code_info"
      (Pbrt.Pp.pp_option pp_source_code_info)
      fmt
      v.Descriptor_types.source_code_info;
    Pbrt.Pp.pp_record_field
      "syntax"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.syntax;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_file_descriptor_set fmt (v : Descriptor_types.file_descriptor_set) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "file"
      (Pbrt.Pp.pp_list pp_file_descriptor_proto)
      fmt
      v.Descriptor_types.file;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_generated_code_info_annotation fmt
    (v : Descriptor_types.generated_code_info_annotation)
  =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "path"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_int)
      fmt
      v.Descriptor_types.path;
    Pbrt.Pp.pp_record_field
      "source_file"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Descriptor_types.source_file;
    Pbrt.Pp.pp_record_field
      "begin_"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_int)
      fmt
      v.Descriptor_types.begin_;
    Pbrt.Pp.pp_record_field
      "end_"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_int)
      fmt
      v.Descriptor_types.end_;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_generated_code_info fmt (v : Descriptor_types.generated_code_info) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "annotation"
      (Pbrt.Pp.pp_list pp_generated_code_info_annotation)
      fmt
      v.Descriptor_types.annotation;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
