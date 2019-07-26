[@@@ocaml.warning "-27-30-39"]

type uninterpreted_option_name_part_mutable = {
  mutable name_part : string;
  mutable is_extension : bool;
}

let default_uninterpreted_option_name_part_mutable ()
    : uninterpreted_option_name_part_mutable
  =
  {name_part = ""; is_extension = false}

type uninterpreted_option_mutable = {
  mutable name : Descriptor_types.uninterpreted_option_name_part list;
  mutable identifier_value : string option;
  mutable positive_int_value : int option;
  mutable negative_int_value : int option;
  mutable double_value : float option;
  mutable string_value : bytes option;
  mutable aggregate_value : string option;
}

let default_uninterpreted_option_mutable () : uninterpreted_option_mutable =
  {
    name = [];
    identifier_value = None;
    positive_int_value = None;
    negative_int_value = None;
    double_value = None;
    string_value = None;
    aggregate_value = None;
  }

type field_options_mutable = {
  mutable ctype : Descriptor_types.field_options_ctype option;
  mutable packed : bool option;
  mutable jstype : Descriptor_types.field_options_jstype option;
  mutable lazy_ : bool option;
  mutable deprecated : bool option;
  mutable weak : bool option;
  mutable uninterpreted_option : Descriptor_types.uninterpreted_option list;
}

let default_field_options_mutable () : field_options_mutable =
  {
    ctype = Some (Descriptor_types.default_field_options_ctype ());
    packed = None;
    jstype = Some (Descriptor_types.default_field_options_jstype ());
    lazy_ = Some false;
    deprecated = Some false;
    weak = Some false;
    uninterpreted_option = [];
  }

type field_descriptor_proto_mutable = {
  mutable name : string option;
  mutable number : int option;
  mutable label : Descriptor_types.field_descriptor_proto_label option;
  mutable type_ : Descriptor_types.field_descriptor_proto_type option;
  mutable type_name : string option;
  mutable extendee : string option;
  mutable default_value : string option;
  mutable oneof_index : int option;
  mutable json_name : string option;
  mutable options : Descriptor_types.field_options option;
}

let default_field_descriptor_proto_mutable () : field_descriptor_proto_mutable =
  {
    name = None;
    number = None;
    label = None;
    type_ = None;
    type_name = None;
    extendee = None;
    default_value = None;
    oneof_index = None;
    json_name = None;
    options = None;
  }

type enum_value_options_mutable = {
  mutable deprecated : bool option;
  mutable uninterpreted_option : Descriptor_types.uninterpreted_option list;
}

let default_enum_value_options_mutable () : enum_value_options_mutable =
  {deprecated = Some false; uninterpreted_option = []}

type enum_value_descriptor_proto_mutable = {
  mutable name : string option;
  mutable number : int option;
  mutable options : Descriptor_types.enum_value_options option;
}

let default_enum_value_descriptor_proto_mutable () : enum_value_descriptor_proto_mutable =
  {name = None; number = None; options = None}

type enum_options_mutable = {
  mutable allow_alias : bool option;
  mutable deprecated : bool option;
  mutable uninterpreted_option : Descriptor_types.uninterpreted_option list;
}

let default_enum_options_mutable () : enum_options_mutable =
  {allow_alias = None; deprecated = Some false; uninterpreted_option = []}

type enum_descriptor_proto_enum_reserved_range_mutable = {
  mutable start : int option;
  mutable end_ : int option;
}

let default_enum_descriptor_proto_enum_reserved_range_mutable ()
    : enum_descriptor_proto_enum_reserved_range_mutable
  =
  {start = None; end_ = None}

type enum_descriptor_proto_mutable = {
  mutable name : string option;
  mutable value : Descriptor_types.enum_value_descriptor_proto list;
  mutable options : Descriptor_types.enum_options option;
  mutable reserved_range :
    Descriptor_types.enum_descriptor_proto_enum_reserved_range list;
  mutable reserved_name : string list;
}

let default_enum_descriptor_proto_mutable () : enum_descriptor_proto_mutable =
  {name = None; value = []; options = None; reserved_range = []; reserved_name = []}

type extension_range_options_mutable = {
  mutable uninterpreted_option : Descriptor_types.uninterpreted_option list;
}

let default_extension_range_options_mutable () : extension_range_options_mutable =
  {uninterpreted_option = []}

type descriptor_proto_extension_range_mutable = {
  mutable start : int option;
  mutable end_ : int option;
  mutable options : Descriptor_types.extension_range_options option;
}

let default_descriptor_proto_extension_range_mutable ()
    : descriptor_proto_extension_range_mutable
  =
  {start = None; end_ = None; options = None}

type oneof_options_mutable = {
  mutable uninterpreted_option : Descriptor_types.uninterpreted_option list;
}

let default_oneof_options_mutable () : oneof_options_mutable =
  {uninterpreted_option = []}

type oneof_descriptor_proto_mutable = {
  mutable name : string option;
  mutable options : Descriptor_types.oneof_options option;
}

let default_oneof_descriptor_proto_mutable () : oneof_descriptor_proto_mutable =
  {name = None; options = None}

type message_options_mutable = {
  mutable message_set_wire_format : bool option;
  mutable no_standard_descriptor_accessor : bool option;
  mutable deprecated : bool option;
  mutable map_entry : bool option;
  mutable uninterpreted_option : Descriptor_types.uninterpreted_option list;
}

let default_message_options_mutable () : message_options_mutable =
  {
    message_set_wire_format = Some false;
    no_standard_descriptor_accessor = Some false;
    deprecated = Some false;
    map_entry = None;
    uninterpreted_option = [];
  }

type descriptor_proto_reserved_range_mutable = {
  mutable start : int option;
  mutable end_ : int option;
}

let default_descriptor_proto_reserved_range_mutable ()
    : descriptor_proto_reserved_range_mutable
  =
  {start = None; end_ = None}

type descriptor_proto_mutable = {
  mutable name : string option;
  mutable field : Descriptor_types.field_descriptor_proto list;
  mutable extension : Descriptor_types.field_descriptor_proto list;
  mutable nested_type : Descriptor_types.descriptor_proto list;
  mutable enum_type : Descriptor_types.enum_descriptor_proto list;
  mutable extension_range : Descriptor_types.descriptor_proto_extension_range list;
  mutable oneof_decl : Descriptor_types.oneof_descriptor_proto list;
  mutable options : Descriptor_types.message_options option;
  mutable reserved_range : Descriptor_types.descriptor_proto_reserved_range list;
  mutable reserved_name : string list;
}

let default_descriptor_proto_mutable () : descriptor_proto_mutable =
  {
    name = None;
    field = [];
    extension = [];
    nested_type = [];
    enum_type = [];
    extension_range = [];
    oneof_decl = [];
    options = None;
    reserved_range = [];
    reserved_name = [];
  }

type method_options_mutable = {
  mutable deprecated : bool option;
  mutable idempotency_level : Descriptor_types.method_options_idempotency_level option;
  mutable uninterpreted_option : Descriptor_types.uninterpreted_option list;
}

let default_method_options_mutable () : method_options_mutable =
  {
    deprecated = Some false;
    idempotency_level =
      Some (Descriptor_types.default_method_options_idempotency_level ());
    uninterpreted_option = [];
  }

type method_descriptor_proto_mutable = {
  mutable name : string option;
  mutable input_type : string option;
  mutable output_type : string option;
  mutable options : Descriptor_types.method_options option;
  mutable client_streaming : bool option;
  mutable server_streaming : bool option;
}

let default_method_descriptor_proto_mutable () : method_descriptor_proto_mutable =
  {
    name = None;
    input_type = None;
    output_type = None;
    options = None;
    client_streaming = Some false;
    server_streaming = Some false;
  }

type service_options_mutable = {
  mutable deprecated : bool option;
  mutable uninterpreted_option : Descriptor_types.uninterpreted_option list;
}

let default_service_options_mutable () : service_options_mutable =
  {deprecated = Some false; uninterpreted_option = []}

type service_descriptor_proto_mutable = {
  mutable name : string option;
  mutable method_ : Descriptor_types.method_descriptor_proto list;
  mutable options : Descriptor_types.service_options option;
}

let default_service_descriptor_proto_mutable () : service_descriptor_proto_mutable =
  {name = None; method_ = []; options = None}

type file_options_mutable = {
  mutable java_package : string option;
  mutable java_outer_classname : string option;
  mutable java_multiple_files : bool option;
  mutable java_generate_equals_and_hash : bool option;
  mutable java_string_check_utf8 : bool option;
  mutable optimize_for : Descriptor_types.file_options_optimize_mode option;
  mutable go_package : string option;
  mutable cc_generic_services : bool option;
  mutable java_generic_services : bool option;
  mutable py_generic_services : bool option;
  mutable php_generic_services : bool option;
  mutable deprecated : bool option;
  mutable cc_enable_arenas : bool option;
  mutable objc_class_prefix : string option;
  mutable csharp_namespace : string option;
  mutable swift_prefix : string option;
  mutable php_class_prefix : string option;
  mutable php_namespace : string option;
  mutable php_metadata_namespace : string option;
  mutable ruby_package : string option;
  mutable uninterpreted_option : Descriptor_types.uninterpreted_option list;
}

let default_file_options_mutable () : file_options_mutable =
  {
    java_package = None;
    java_outer_classname = None;
    java_multiple_files = Some false;
    java_generate_equals_and_hash = None;
    java_string_check_utf8 = Some false;
    optimize_for = Some (Descriptor_types.default_file_options_optimize_mode ());
    go_package = None;
    cc_generic_services = Some false;
    java_generic_services = Some false;
    py_generic_services = Some false;
    php_generic_services = Some false;
    deprecated = Some false;
    cc_enable_arenas = Some false;
    objc_class_prefix = None;
    csharp_namespace = None;
    swift_prefix = None;
    php_class_prefix = None;
    php_namespace = None;
    php_metadata_namespace = None;
    ruby_package = None;
    uninterpreted_option = [];
  }

type source_code_info_location_mutable = {
  mutable path : int list;
  mutable span : int list;
  mutable leading_comments : string option;
  mutable trailing_comments : string option;
  mutable leading_detached_comments : string list;
}

let default_source_code_info_location_mutable () : source_code_info_location_mutable =
  {
    path = [];
    span = [];
    leading_comments = None;
    trailing_comments = None;
    leading_detached_comments = [];
  }

type source_code_info_mutable = {
  mutable location : Descriptor_types.source_code_info_location list;
}

let default_source_code_info_mutable () : source_code_info_mutable = {location = []}

type file_descriptor_proto_mutable = {
  mutable name : string option;
  mutable package : string option;
  mutable dependency : string list;
  mutable public_dependency : int list;
  mutable weak_dependency : int list;
  mutable message_type : Descriptor_types.descriptor_proto list;
  mutable enum_type : Descriptor_types.enum_descriptor_proto list;
  mutable service : Descriptor_types.service_descriptor_proto list;
  mutable extension : Descriptor_types.field_descriptor_proto list;
  mutable options : Descriptor_types.file_options option;
  mutable source_code_info : Descriptor_types.source_code_info option;
  mutable syntax : string option;
}

let default_file_descriptor_proto_mutable () : file_descriptor_proto_mutable =
  {
    name = None;
    package = None;
    dependency = [];
    public_dependency = [];
    weak_dependency = [];
    message_type = [];
    enum_type = [];
    service = [];
    extension = [];
    options = None;
    source_code_info = None;
    syntax = None;
  }

type file_descriptor_set_mutable = {
  mutable file : Descriptor_types.file_descriptor_proto list;
}

let default_file_descriptor_set_mutable () : file_descriptor_set_mutable = {file = []}

type generated_code_info_annotation_mutable = {
  mutable path : int list;
  mutable source_file : string option;
  mutable begin_ : int option;
  mutable end_ : int option;
}

let default_generated_code_info_annotation_mutable ()
    : generated_code_info_annotation_mutable
  =
  {path = []; source_file = None; begin_ = None; end_ = None}

type generated_code_info_mutable = {
  mutable annotation : Descriptor_types.generated_code_info_annotation list;
}

let default_generated_code_info_mutable () : generated_code_info_mutable =
  {annotation = []}

let rec decode_field_descriptor_proto_label d =
  match Pbrt.Decoder.int_as_varint d with
  | 1 ->
      (Descriptor_types.Label_optional : Descriptor_types.field_descriptor_proto_label)
  | 2 ->
      (Descriptor_types.Label_required : Descriptor_types.field_descriptor_proto_label)
  | 3 ->
      (Descriptor_types.Label_repeated : Descriptor_types.field_descriptor_proto_label)
  | _ -> Pbrt.Decoder.malformed_variant "field_descriptor_proto_label"

let rec decode_field_descriptor_proto_type d =
  match Pbrt.Decoder.int_as_varint d with
  | 1 -> (Descriptor_types.Type_double : Descriptor_types.field_descriptor_proto_type)
  | 2 -> (Descriptor_types.Type_float : Descriptor_types.field_descriptor_proto_type)
  | 3 -> (Descriptor_types.Type_int64 : Descriptor_types.field_descriptor_proto_type)
  | 4 -> (Descriptor_types.Type_uint64 : Descriptor_types.field_descriptor_proto_type)
  | 5 -> (Descriptor_types.Type_int32 : Descriptor_types.field_descriptor_proto_type)
  | 6 -> (Descriptor_types.Type_fixed64 : Descriptor_types.field_descriptor_proto_type)
  | 7 -> (Descriptor_types.Type_fixed32 : Descriptor_types.field_descriptor_proto_type)
  | 8 -> (Descriptor_types.Type_bool : Descriptor_types.field_descriptor_proto_type)
  | 9 -> (Descriptor_types.Type_string : Descriptor_types.field_descriptor_proto_type)
  | 10 -> (Descriptor_types.Type_group : Descriptor_types.field_descriptor_proto_type)
  | 11 -> (Descriptor_types.Type_message : Descriptor_types.field_descriptor_proto_type)
  | 12 -> (Descriptor_types.Type_bytes : Descriptor_types.field_descriptor_proto_type)
  | 13 -> (Descriptor_types.Type_uint32 : Descriptor_types.field_descriptor_proto_type)
  | 14 -> (Descriptor_types.Type_enum : Descriptor_types.field_descriptor_proto_type)
  | 15 -> (Descriptor_types.Type_sfixed32 : Descriptor_types.field_descriptor_proto_type)
  | 16 -> (Descriptor_types.Type_sfixed64 : Descriptor_types.field_descriptor_proto_type)
  | 17 -> (Descriptor_types.Type_sint32 : Descriptor_types.field_descriptor_proto_type)
  | 18 -> (Descriptor_types.Type_sint64 : Descriptor_types.field_descriptor_proto_type)
  | _ -> Pbrt.Decoder.malformed_variant "field_descriptor_proto_type"

let rec decode_field_options_ctype d =
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Descriptor_types.String : Descriptor_types.field_options_ctype)
  | 1 -> (Descriptor_types.Cord : Descriptor_types.field_options_ctype)
  | 2 -> (Descriptor_types.String_piece : Descriptor_types.field_options_ctype)
  | _ -> Pbrt.Decoder.malformed_variant "field_options_ctype"

let rec decode_field_options_jstype d =
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Descriptor_types.Js_normal : Descriptor_types.field_options_jstype)
  | 1 -> (Descriptor_types.Js_string : Descriptor_types.field_options_jstype)
  | 2 -> (Descriptor_types.Js_number : Descriptor_types.field_options_jstype)
  | _ -> Pbrt.Decoder.malformed_variant "field_options_jstype"

let rec decode_uninterpreted_option_name_part d =
  let v = default_uninterpreted_option_name_part_mutable () in
  let continue__ = ref true in
  let is_extension_is_set = ref false in
  let name_part_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        ();
        continue__ := false
    | Some (1, Pbrt.Bytes) ->
        v.name_part <- Pbrt.Decoder.string d;
        name_part_is_set := true
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload
          "Message(uninterpreted_option_name_part), field(1)"
          pk
    | Some (2, Pbrt.Varint) ->
        v.is_extension <- Pbrt.Decoder.bool d;
        is_extension_is_set := true
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload
          "Message(uninterpreted_option_name_part), field(2)"
          pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  if not !is_extension_is_set then Pbrt.Decoder.missing_field "is_extension";
  if not !name_part_is_set then Pbrt.Decoder.missing_field "name_part";
  ({
     Descriptor_types.name_part = v.name_part;
     Descriptor_types.is_extension = v.is_extension;
   }
    : Descriptor_types.uninterpreted_option_name_part)

let rec decode_uninterpreted_option d =
  let v = default_uninterpreted_option_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.name <- List.rev v.name;
        continue__ := false
    | Some (2, Pbrt.Bytes) ->
        v.name <- decode_uninterpreted_option_name_part (Pbrt.Decoder.nested d) :: v.name
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(uninterpreted_option), field(2)" pk
    | Some (3, Pbrt.Bytes) -> v.identifier_value <- Some (Pbrt.Decoder.string d)
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(uninterpreted_option), field(3)" pk
    | Some (4, Pbrt.Varint) ->
        v.positive_int_value <- Some (Pbrt.Decoder.int_as_varint d)
    | Some (4, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(uninterpreted_option), field(4)" pk
    | Some (5, Pbrt.Varint) ->
        v.negative_int_value <- Some (Pbrt.Decoder.int_as_varint d)
    | Some (5, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(uninterpreted_option), field(5)" pk
    | Some (6, Pbrt.Bits64) -> v.double_value <- Some (Pbrt.Decoder.float_as_bits64 d)
    | Some (6, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(uninterpreted_option), field(6)" pk
    | Some (7, Pbrt.Bytes) -> v.string_value <- Some (Pbrt.Decoder.bytes d)
    | Some (7, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(uninterpreted_option), field(7)" pk
    | Some (8, Pbrt.Bytes) -> v.aggregate_value <- Some (Pbrt.Decoder.string d)
    | Some (8, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(uninterpreted_option), field(8)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Descriptor_types.name = v.name;
     Descriptor_types.identifier_value = v.identifier_value;
     Descriptor_types.positive_int_value = v.positive_int_value;
     Descriptor_types.negative_int_value = v.negative_int_value;
     Descriptor_types.double_value = v.double_value;
     Descriptor_types.string_value = v.string_value;
     Descriptor_types.aggregate_value = v.aggregate_value;
   }
    : Descriptor_types.uninterpreted_option)

let rec decode_field_options d =
  let v = default_field_options_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.uninterpreted_option <- List.rev v.uninterpreted_option;
        continue__ := false
    | Some (1, Pbrt.Varint) -> v.ctype <- Some (decode_field_options_ctype d)
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(field_options), field(1)" pk
    | Some (2, Pbrt.Varint) -> v.packed <- Some (Pbrt.Decoder.bool d)
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(field_options), field(2)" pk
    | Some (6, Pbrt.Varint) -> v.jstype <- Some (decode_field_options_jstype d)
    | Some (6, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(field_options), field(6)" pk
    | Some (5, Pbrt.Varint) -> v.lazy_ <- Some (Pbrt.Decoder.bool d)
    | Some (5, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(field_options), field(5)" pk
    | Some (3, Pbrt.Varint) -> v.deprecated <- Some (Pbrt.Decoder.bool d)
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(field_options), field(3)" pk
    | Some (10, Pbrt.Varint) -> v.weak <- Some (Pbrt.Decoder.bool d)
    | Some (10, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(field_options), field(10)" pk
    | Some (999, Pbrt.Bytes) ->
        v.uninterpreted_option <-
          decode_uninterpreted_option (Pbrt.Decoder.nested d) :: v.uninterpreted_option
    | Some (999, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(field_options), field(999)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Descriptor_types.ctype = v.ctype;
     Descriptor_types.packed = v.packed;
     Descriptor_types.jstype = v.jstype;
     Descriptor_types.lazy_ = v.lazy_;
     Descriptor_types.deprecated = v.deprecated;
     Descriptor_types.weak = v.weak;
     Descriptor_types.uninterpreted_option = v.uninterpreted_option;
   }
    : Descriptor_types.field_options)

let rec decode_field_descriptor_proto d =
  let v = default_field_descriptor_proto_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        ();
        continue__ := false
    | Some (1, Pbrt.Bytes) -> v.name <- Some (Pbrt.Decoder.string d)
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(field_descriptor_proto), field(1)" pk
    | Some (3, Pbrt.Varint) -> v.number <- Some (Pbrt.Decoder.int_as_varint d)
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(field_descriptor_proto), field(3)" pk
    | Some (4, Pbrt.Varint) -> v.label <- Some (decode_field_descriptor_proto_label d)
    | Some (4, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(field_descriptor_proto), field(4)" pk
    | Some (5, Pbrt.Varint) -> v.type_ <- Some (decode_field_descriptor_proto_type d)
    | Some (5, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(field_descriptor_proto), field(5)" pk
    | Some (6, Pbrt.Bytes) -> v.type_name <- Some (Pbrt.Decoder.string d)
    | Some (6, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(field_descriptor_proto), field(6)" pk
    | Some (2, Pbrt.Bytes) -> v.extendee <- Some (Pbrt.Decoder.string d)
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(field_descriptor_proto), field(2)" pk
    | Some (7, Pbrt.Bytes) -> v.default_value <- Some (Pbrt.Decoder.string d)
    | Some (7, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(field_descriptor_proto), field(7)" pk
    | Some (9, Pbrt.Varint) -> v.oneof_index <- Some (Pbrt.Decoder.int_as_varint d)
    | Some (9, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(field_descriptor_proto), field(9)" pk
    | Some (10, Pbrt.Bytes) -> v.json_name <- Some (Pbrt.Decoder.string d)
    | Some (10, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(field_descriptor_proto), field(10)" pk
    | Some (8, Pbrt.Bytes) ->
        v.options <- Some (decode_field_options (Pbrt.Decoder.nested d))
    | Some (8, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(field_descriptor_proto), field(8)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Descriptor_types.name = v.name;
     Descriptor_types.number = v.number;
     Descriptor_types.label = v.label;
     Descriptor_types.type_ = v.type_;
     Descriptor_types.type_name = v.type_name;
     Descriptor_types.extendee = v.extendee;
     Descriptor_types.default_value = v.default_value;
     Descriptor_types.oneof_index = v.oneof_index;
     Descriptor_types.json_name = v.json_name;
     Descriptor_types.options = v.options;
   }
    : Descriptor_types.field_descriptor_proto)

let rec decode_enum_value_options d =
  let v = default_enum_value_options_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.uninterpreted_option <- List.rev v.uninterpreted_option;
        continue__ := false
    | Some (1, Pbrt.Varint) -> v.deprecated <- Some (Pbrt.Decoder.bool d)
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(enum_value_options), field(1)" pk
    | Some (999, Pbrt.Bytes) ->
        v.uninterpreted_option <-
          decode_uninterpreted_option (Pbrt.Decoder.nested d) :: v.uninterpreted_option
    | Some (999, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(enum_value_options), field(999)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Descriptor_types.deprecated = v.deprecated;
     Descriptor_types.uninterpreted_option = v.uninterpreted_option;
   }
    : Descriptor_types.enum_value_options)

let rec decode_enum_value_descriptor_proto d =
  let v = default_enum_value_descriptor_proto_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        ();
        continue__ := false
    | Some (1, Pbrt.Bytes) -> v.name <- Some (Pbrt.Decoder.string d)
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload
          "Message(enum_value_descriptor_proto), field(1)"
          pk
    | Some (2, Pbrt.Varint) -> v.number <- Some (Pbrt.Decoder.int_as_varint d)
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload
          "Message(enum_value_descriptor_proto), field(2)"
          pk
    | Some (3, Pbrt.Bytes) ->
        v.options <- Some (decode_enum_value_options (Pbrt.Decoder.nested d))
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload
          "Message(enum_value_descriptor_proto), field(3)"
          pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Descriptor_types.name = v.name;
     Descriptor_types.number = v.number;
     Descriptor_types.options = v.options;
   }
    : Descriptor_types.enum_value_descriptor_proto)

let rec decode_enum_options d =
  let v = default_enum_options_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.uninterpreted_option <- List.rev v.uninterpreted_option;
        continue__ := false
    | Some (2, Pbrt.Varint) -> v.allow_alias <- Some (Pbrt.Decoder.bool d)
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(enum_options), field(2)" pk
    | Some (3, Pbrt.Varint) -> v.deprecated <- Some (Pbrt.Decoder.bool d)
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(enum_options), field(3)" pk
    | Some (999, Pbrt.Bytes) ->
        v.uninterpreted_option <-
          decode_uninterpreted_option (Pbrt.Decoder.nested d) :: v.uninterpreted_option
    | Some (999, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(enum_options), field(999)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Descriptor_types.allow_alias = v.allow_alias;
     Descriptor_types.deprecated = v.deprecated;
     Descriptor_types.uninterpreted_option = v.uninterpreted_option;
   }
    : Descriptor_types.enum_options)

let rec decode_enum_descriptor_proto_enum_reserved_range d =
  let v = default_enum_descriptor_proto_enum_reserved_range_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        ();
        continue__ := false
    | Some (1, Pbrt.Varint) -> v.start <- Some (Pbrt.Decoder.int_as_varint d)
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload
          "Message(enum_descriptor_proto_enum_reserved_range), field(1)"
          pk
    | Some (2, Pbrt.Varint) -> v.end_ <- Some (Pbrt.Decoder.int_as_varint d)
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload
          "Message(enum_descriptor_proto_enum_reserved_range), field(2)"
          pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({Descriptor_types.start = v.start; Descriptor_types.end_ = v.end_}
    : Descriptor_types.enum_descriptor_proto_enum_reserved_range)

let rec decode_enum_descriptor_proto d =
  let v = default_enum_descriptor_proto_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.reserved_name <- List.rev v.reserved_name;
        v.reserved_range <- List.rev v.reserved_range;
        v.value <- List.rev v.value;
        continue__ := false
    | Some (1, Pbrt.Bytes) -> v.name <- Some (Pbrt.Decoder.string d)
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(enum_descriptor_proto), field(1)" pk
    | Some (2, Pbrt.Bytes) ->
        v.value <- decode_enum_value_descriptor_proto (Pbrt.Decoder.nested d) :: v.value
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(enum_descriptor_proto), field(2)" pk
    | Some (3, Pbrt.Bytes) ->
        v.options <- Some (decode_enum_options (Pbrt.Decoder.nested d))
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(enum_descriptor_proto), field(3)" pk
    | Some (4, Pbrt.Bytes) ->
        v.reserved_range <-
          decode_enum_descriptor_proto_enum_reserved_range (Pbrt.Decoder.nested d)
          :: v.reserved_range
    | Some (4, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(enum_descriptor_proto), field(4)" pk
    | Some (5, Pbrt.Bytes) -> v.reserved_name <- Pbrt.Decoder.string d :: v.reserved_name
    | Some (5, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(enum_descriptor_proto), field(5)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Descriptor_types.name = v.name;
     Descriptor_types.value = v.value;
     Descriptor_types.options = v.options;
     Descriptor_types.reserved_range = v.reserved_range;
     Descriptor_types.reserved_name = v.reserved_name;
   }
    : Descriptor_types.enum_descriptor_proto)

let rec decode_extension_range_options d =
  let v = default_extension_range_options_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.uninterpreted_option <- List.rev v.uninterpreted_option;
        continue__ := false
    | Some (999, Pbrt.Bytes) ->
        v.uninterpreted_option <-
          decode_uninterpreted_option (Pbrt.Decoder.nested d) :: v.uninterpreted_option
    | Some (999, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(extension_range_options), field(999)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({Descriptor_types.uninterpreted_option = v.uninterpreted_option}
    : Descriptor_types.extension_range_options)

let rec decode_descriptor_proto_extension_range d =
  let v = default_descriptor_proto_extension_range_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        ();
        continue__ := false
    | Some (1, Pbrt.Varint) -> v.start <- Some (Pbrt.Decoder.int_as_varint d)
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload
          "Message(descriptor_proto_extension_range), field(1)"
          pk
    | Some (2, Pbrt.Varint) -> v.end_ <- Some (Pbrt.Decoder.int_as_varint d)
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload
          "Message(descriptor_proto_extension_range), field(2)"
          pk
    | Some (3, Pbrt.Bytes) ->
        v.options <- Some (decode_extension_range_options (Pbrt.Decoder.nested d))
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload
          "Message(descriptor_proto_extension_range), field(3)"
          pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Descriptor_types.start = v.start;
     Descriptor_types.end_ = v.end_;
     Descriptor_types.options = v.options;
   }
    : Descriptor_types.descriptor_proto_extension_range)

let rec decode_oneof_options d =
  let v = default_oneof_options_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.uninterpreted_option <- List.rev v.uninterpreted_option;
        continue__ := false
    | Some (999, Pbrt.Bytes) ->
        v.uninterpreted_option <-
          decode_uninterpreted_option (Pbrt.Decoder.nested d) :: v.uninterpreted_option
    | Some (999, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(oneof_options), field(999)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({Descriptor_types.uninterpreted_option = v.uninterpreted_option}
    : Descriptor_types.oneof_options)

let rec decode_oneof_descriptor_proto d =
  let v = default_oneof_descriptor_proto_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        ();
        continue__ := false
    | Some (1, Pbrt.Bytes) -> v.name <- Some (Pbrt.Decoder.string d)
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(oneof_descriptor_proto), field(1)" pk
    | Some (2, Pbrt.Bytes) ->
        v.options <- Some (decode_oneof_options (Pbrt.Decoder.nested d))
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(oneof_descriptor_proto), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({Descriptor_types.name = v.name; Descriptor_types.options = v.options}
    : Descriptor_types.oneof_descriptor_proto)

let rec decode_message_options d =
  let v = default_message_options_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.uninterpreted_option <- List.rev v.uninterpreted_option;
        continue__ := false
    | Some (1, Pbrt.Varint) -> v.message_set_wire_format <- Some (Pbrt.Decoder.bool d)
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(message_options), field(1)" pk
    | Some (2, Pbrt.Varint) ->
        v.no_standard_descriptor_accessor <- Some (Pbrt.Decoder.bool d)
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(message_options), field(2)" pk
    | Some (3, Pbrt.Varint) -> v.deprecated <- Some (Pbrt.Decoder.bool d)
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(message_options), field(3)" pk
    | Some (7, Pbrt.Varint) -> v.map_entry <- Some (Pbrt.Decoder.bool d)
    | Some (7, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(message_options), field(7)" pk
    | Some (999, Pbrt.Bytes) ->
        v.uninterpreted_option <-
          decode_uninterpreted_option (Pbrt.Decoder.nested d) :: v.uninterpreted_option
    | Some (999, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(message_options), field(999)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Descriptor_types.message_set_wire_format = v.message_set_wire_format;
     Descriptor_types.no_standard_descriptor_accessor = v.no_standard_descriptor_accessor;
     Descriptor_types.deprecated = v.deprecated;
     Descriptor_types.map_entry = v.map_entry;
     Descriptor_types.uninterpreted_option = v.uninterpreted_option;
   }
    : Descriptor_types.message_options)

let rec decode_descriptor_proto_reserved_range d =
  let v = default_descriptor_proto_reserved_range_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        ();
        continue__ := false
    | Some (1, Pbrt.Varint) -> v.start <- Some (Pbrt.Decoder.int_as_varint d)
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload
          "Message(descriptor_proto_reserved_range), field(1)"
          pk
    | Some (2, Pbrt.Varint) -> v.end_ <- Some (Pbrt.Decoder.int_as_varint d)
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload
          "Message(descriptor_proto_reserved_range), field(2)"
          pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({Descriptor_types.start = v.start; Descriptor_types.end_ = v.end_}
    : Descriptor_types.descriptor_proto_reserved_range)

let rec decode_descriptor_proto d =
  let v = default_descriptor_proto_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.reserved_name <- List.rev v.reserved_name;
        v.reserved_range <- List.rev v.reserved_range;
        v.oneof_decl <- List.rev v.oneof_decl;
        v.extension_range <- List.rev v.extension_range;
        v.enum_type <- List.rev v.enum_type;
        v.nested_type <- List.rev v.nested_type;
        v.extension <- List.rev v.extension;
        v.field <- List.rev v.field;
        continue__ := false
    | Some (1, Pbrt.Bytes) -> v.name <- Some (Pbrt.Decoder.string d)
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(descriptor_proto), field(1)" pk
    | Some (2, Pbrt.Bytes) ->
        v.field <- decode_field_descriptor_proto (Pbrt.Decoder.nested d) :: v.field
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(descriptor_proto), field(2)" pk
    | Some (6, Pbrt.Bytes) ->
        v.extension <-
          decode_field_descriptor_proto (Pbrt.Decoder.nested d) :: v.extension
    | Some (6, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(descriptor_proto), field(6)" pk
    | Some (3, Pbrt.Bytes) ->
        v.nested_type <- decode_descriptor_proto (Pbrt.Decoder.nested d) :: v.nested_type
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(descriptor_proto), field(3)" pk
    | Some (4, Pbrt.Bytes) ->
        v.enum_type <-
          decode_enum_descriptor_proto (Pbrt.Decoder.nested d) :: v.enum_type
    | Some (4, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(descriptor_proto), field(4)" pk
    | Some (5, Pbrt.Bytes) ->
        v.extension_range <-
          decode_descriptor_proto_extension_range (Pbrt.Decoder.nested d)
          :: v.extension_range
    | Some (5, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(descriptor_proto), field(5)" pk
    | Some (8, Pbrt.Bytes) ->
        v.oneof_decl <-
          decode_oneof_descriptor_proto (Pbrt.Decoder.nested d) :: v.oneof_decl
    | Some (8, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(descriptor_proto), field(8)" pk
    | Some (7, Pbrt.Bytes) ->
        v.options <- Some (decode_message_options (Pbrt.Decoder.nested d))
    | Some (7, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(descriptor_proto), field(7)" pk
    | Some (9, Pbrt.Bytes) ->
        v.reserved_range <-
          decode_descriptor_proto_reserved_range (Pbrt.Decoder.nested d)
          :: v.reserved_range
    | Some (9, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(descriptor_proto), field(9)" pk
    | Some (10, Pbrt.Bytes) ->
        v.reserved_name <- Pbrt.Decoder.string d :: v.reserved_name
    | Some (10, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(descriptor_proto), field(10)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Descriptor_types.name = v.name;
     Descriptor_types.field = v.field;
     Descriptor_types.extension = v.extension;
     Descriptor_types.nested_type = v.nested_type;
     Descriptor_types.enum_type = v.enum_type;
     Descriptor_types.extension_range = v.extension_range;
     Descriptor_types.oneof_decl = v.oneof_decl;
     Descriptor_types.options = v.options;
     Descriptor_types.reserved_range = v.reserved_range;
     Descriptor_types.reserved_name = v.reserved_name;
   }
    : Descriptor_types.descriptor_proto)

let rec decode_method_options_idempotency_level d =
  match Pbrt.Decoder.int_as_varint d with
  | 0 ->
      (Descriptor_types.Idempotency_unknown
        : Descriptor_types.method_options_idempotency_level)
  | 1 ->
      (Descriptor_types.No_side_effects
        : Descriptor_types.method_options_idempotency_level)
  | 2 ->
      (Descriptor_types.Idempotent : Descriptor_types.method_options_idempotency_level)
  | _ -> Pbrt.Decoder.malformed_variant "method_options_idempotency_level"

let rec decode_method_options d =
  let v = default_method_options_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.uninterpreted_option <- List.rev v.uninterpreted_option;
        continue__ := false
    | Some (33, Pbrt.Varint) -> v.deprecated <- Some (Pbrt.Decoder.bool d)
    | Some (33, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(method_options), field(33)" pk
    | Some (34, Pbrt.Varint) ->
        v.idempotency_level <- Some (decode_method_options_idempotency_level d)
    | Some (34, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(method_options), field(34)" pk
    | Some (999, Pbrt.Bytes) ->
        v.uninterpreted_option <-
          decode_uninterpreted_option (Pbrt.Decoder.nested d) :: v.uninterpreted_option
    | Some (999, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(method_options), field(999)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Descriptor_types.deprecated = v.deprecated;
     Descriptor_types.idempotency_level = v.idempotency_level;
     Descriptor_types.uninterpreted_option = v.uninterpreted_option;
   }
    : Descriptor_types.method_options)

let rec decode_method_descriptor_proto d =
  let v = default_method_descriptor_proto_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        ();
        continue__ := false
    | Some (1, Pbrt.Bytes) -> v.name <- Some (Pbrt.Decoder.string d)
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(method_descriptor_proto), field(1)" pk
    | Some (2, Pbrt.Bytes) -> v.input_type <- Some (Pbrt.Decoder.string d)
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(method_descriptor_proto), field(2)" pk
    | Some (3, Pbrt.Bytes) -> v.output_type <- Some (Pbrt.Decoder.string d)
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(method_descriptor_proto), field(3)" pk
    | Some (4, Pbrt.Bytes) ->
        v.options <- Some (decode_method_options (Pbrt.Decoder.nested d))
    | Some (4, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(method_descriptor_proto), field(4)" pk
    | Some (5, Pbrt.Varint) -> v.client_streaming <- Some (Pbrt.Decoder.bool d)
    | Some (5, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(method_descriptor_proto), field(5)" pk
    | Some (6, Pbrt.Varint) -> v.server_streaming <- Some (Pbrt.Decoder.bool d)
    | Some (6, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(method_descriptor_proto), field(6)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Descriptor_types.name = v.name;
     Descriptor_types.input_type = v.input_type;
     Descriptor_types.output_type = v.output_type;
     Descriptor_types.options = v.options;
     Descriptor_types.client_streaming = v.client_streaming;
     Descriptor_types.server_streaming = v.server_streaming;
   }
    : Descriptor_types.method_descriptor_proto)

let rec decode_service_options d =
  let v = default_service_options_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.uninterpreted_option <- List.rev v.uninterpreted_option;
        continue__ := false
    | Some (33, Pbrt.Varint) -> v.deprecated <- Some (Pbrt.Decoder.bool d)
    | Some (33, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(service_options), field(33)" pk
    | Some (999, Pbrt.Bytes) ->
        v.uninterpreted_option <-
          decode_uninterpreted_option (Pbrt.Decoder.nested d) :: v.uninterpreted_option
    | Some (999, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(service_options), field(999)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Descriptor_types.deprecated = v.deprecated;
     Descriptor_types.uninterpreted_option = v.uninterpreted_option;
   }
    : Descriptor_types.service_options)

let rec decode_service_descriptor_proto d =
  let v = default_service_descriptor_proto_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.method_ <- List.rev v.method_;
        continue__ := false
    | Some (1, Pbrt.Bytes) -> v.name <- Some (Pbrt.Decoder.string d)
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(service_descriptor_proto), field(1)" pk
    | Some (2, Pbrt.Bytes) ->
        v.method_ <- decode_method_descriptor_proto (Pbrt.Decoder.nested d) :: v.method_
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(service_descriptor_proto), field(2)" pk
    | Some (3, Pbrt.Bytes) ->
        v.options <- Some (decode_service_options (Pbrt.Decoder.nested d))
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(service_descriptor_proto), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Descriptor_types.name = v.name;
     Descriptor_types.method_ = v.method_;
     Descriptor_types.options = v.options;
   }
    : Descriptor_types.service_descriptor_proto)

let rec decode_file_options_optimize_mode d =
  match Pbrt.Decoder.int_as_varint d with
  | 1 -> (Descriptor_types.Speed : Descriptor_types.file_options_optimize_mode)
  | 2 -> (Descriptor_types.Code_size : Descriptor_types.file_options_optimize_mode)
  | 3 -> (Descriptor_types.Lite_runtime : Descriptor_types.file_options_optimize_mode)
  | _ -> Pbrt.Decoder.malformed_variant "file_options_optimize_mode"

let rec decode_file_options d =
  let v = default_file_options_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.uninterpreted_option <- List.rev v.uninterpreted_option;
        continue__ := false
    | Some (1, Pbrt.Bytes) -> v.java_package <- Some (Pbrt.Decoder.string d)
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_options), field(1)" pk
    | Some (8, Pbrt.Bytes) -> v.java_outer_classname <- Some (Pbrt.Decoder.string d)
    | Some (8, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_options), field(8)" pk
    | Some (10, Pbrt.Varint) -> v.java_multiple_files <- Some (Pbrt.Decoder.bool d)
    | Some (10, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_options), field(10)" pk
    | Some (20, Pbrt.Varint) ->
        v.java_generate_equals_and_hash <- Some (Pbrt.Decoder.bool d)
    | Some (20, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_options), field(20)" pk
    | Some (27, Pbrt.Varint) -> v.java_string_check_utf8 <- Some (Pbrt.Decoder.bool d)
    | Some (27, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_options), field(27)" pk
    | Some (9, Pbrt.Varint) ->
        v.optimize_for <- Some (decode_file_options_optimize_mode d)
    | Some (9, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_options), field(9)" pk
    | Some (11, Pbrt.Bytes) -> v.go_package <- Some (Pbrt.Decoder.string d)
    | Some (11, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_options), field(11)" pk
    | Some (16, Pbrt.Varint) -> v.cc_generic_services <- Some (Pbrt.Decoder.bool d)
    | Some (16, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_options), field(16)" pk
    | Some (17, Pbrt.Varint) -> v.java_generic_services <- Some (Pbrt.Decoder.bool d)
    | Some (17, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_options), field(17)" pk
    | Some (18, Pbrt.Varint) -> v.py_generic_services <- Some (Pbrt.Decoder.bool d)
    | Some (18, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_options), field(18)" pk
    | Some (42, Pbrt.Varint) -> v.php_generic_services <- Some (Pbrt.Decoder.bool d)
    | Some (42, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_options), field(42)" pk
    | Some (23, Pbrt.Varint) -> v.deprecated <- Some (Pbrt.Decoder.bool d)
    | Some (23, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_options), field(23)" pk
    | Some (31, Pbrt.Varint) -> v.cc_enable_arenas <- Some (Pbrt.Decoder.bool d)
    | Some (31, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_options), field(31)" pk
    | Some (36, Pbrt.Bytes) -> v.objc_class_prefix <- Some (Pbrt.Decoder.string d)
    | Some (36, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_options), field(36)" pk
    | Some (37, Pbrt.Bytes) -> v.csharp_namespace <- Some (Pbrt.Decoder.string d)
    | Some (37, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_options), field(37)" pk
    | Some (39, Pbrt.Bytes) -> v.swift_prefix <- Some (Pbrt.Decoder.string d)
    | Some (39, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_options), field(39)" pk
    | Some (40, Pbrt.Bytes) -> v.php_class_prefix <- Some (Pbrt.Decoder.string d)
    | Some (40, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_options), field(40)" pk
    | Some (41, Pbrt.Bytes) -> v.php_namespace <- Some (Pbrt.Decoder.string d)
    | Some (41, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_options), field(41)" pk
    | Some (44, Pbrt.Bytes) -> v.php_metadata_namespace <- Some (Pbrt.Decoder.string d)
    | Some (44, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_options), field(44)" pk
    | Some (45, Pbrt.Bytes) -> v.ruby_package <- Some (Pbrt.Decoder.string d)
    | Some (45, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_options), field(45)" pk
    | Some (999, Pbrt.Bytes) ->
        v.uninterpreted_option <-
          decode_uninterpreted_option (Pbrt.Decoder.nested d) :: v.uninterpreted_option
    | Some (999, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_options), field(999)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Descriptor_types.java_package = v.java_package;
     Descriptor_types.java_outer_classname = v.java_outer_classname;
     Descriptor_types.java_multiple_files = v.java_multiple_files;
     Descriptor_types.java_generate_equals_and_hash = v.java_generate_equals_and_hash;
     Descriptor_types.java_string_check_utf8 = v.java_string_check_utf8;
     Descriptor_types.optimize_for = v.optimize_for;
     Descriptor_types.go_package = v.go_package;
     Descriptor_types.cc_generic_services = v.cc_generic_services;
     Descriptor_types.java_generic_services = v.java_generic_services;
     Descriptor_types.py_generic_services = v.py_generic_services;
     Descriptor_types.php_generic_services = v.php_generic_services;
     Descriptor_types.deprecated = v.deprecated;
     Descriptor_types.cc_enable_arenas = v.cc_enable_arenas;
     Descriptor_types.objc_class_prefix = v.objc_class_prefix;
     Descriptor_types.csharp_namespace = v.csharp_namespace;
     Descriptor_types.swift_prefix = v.swift_prefix;
     Descriptor_types.php_class_prefix = v.php_class_prefix;
     Descriptor_types.php_namespace = v.php_namespace;
     Descriptor_types.php_metadata_namespace = v.php_metadata_namespace;
     Descriptor_types.ruby_package = v.ruby_package;
     Descriptor_types.uninterpreted_option = v.uninterpreted_option;
   }
    : Descriptor_types.file_options)

let rec decode_source_code_info_location d =
  let v = default_source_code_info_location_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.leading_detached_comments <- List.rev v.leading_detached_comments;
        v.span <- List.rev v.span;
        v.path <- List.rev v.path;
        continue__ := false
    | Some (1, Pbrt.Bytes) ->
        v.path <-
          Pbrt.Decoder.packed_fold (fun l d -> Pbrt.Decoder.int_as_varint d :: l) [] d
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(source_code_info_location), field(1)" pk
    | Some (2, Pbrt.Bytes) ->
        v.span <-
          Pbrt.Decoder.packed_fold (fun l d -> Pbrt.Decoder.int_as_varint d :: l) [] d
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(source_code_info_location), field(2)" pk
    | Some (3, Pbrt.Bytes) -> v.leading_comments <- Some (Pbrt.Decoder.string d)
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(source_code_info_location), field(3)" pk
    | Some (4, Pbrt.Bytes) -> v.trailing_comments <- Some (Pbrt.Decoder.string d)
    | Some (4, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(source_code_info_location), field(4)" pk
    | Some (6, Pbrt.Bytes) ->
        v.leading_detached_comments <-
          Pbrt.Decoder.string d :: v.leading_detached_comments
    | Some (6, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(source_code_info_location), field(6)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Descriptor_types.path = v.path;
     Descriptor_types.span = v.span;
     Descriptor_types.leading_comments = v.leading_comments;
     Descriptor_types.trailing_comments = v.trailing_comments;
     Descriptor_types.leading_detached_comments = v.leading_detached_comments;
   }
    : Descriptor_types.source_code_info_location)

let rec decode_source_code_info d =
  let v = default_source_code_info_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.location <- List.rev v.location;
        continue__ := false
    | Some (1, Pbrt.Bytes) ->
        v.location <-
          decode_source_code_info_location (Pbrt.Decoder.nested d) :: v.location
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(source_code_info), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({Descriptor_types.location = v.location} : Descriptor_types.source_code_info)

let rec decode_file_descriptor_proto d =
  let v = default_file_descriptor_proto_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.extension <- List.rev v.extension;
        v.service <- List.rev v.service;
        v.enum_type <- List.rev v.enum_type;
        v.message_type <- List.rev v.message_type;
        v.weak_dependency <- List.rev v.weak_dependency;
        v.public_dependency <- List.rev v.public_dependency;
        v.dependency <- List.rev v.dependency;
        continue__ := false
    | Some (1, Pbrt.Bytes) -> v.name <- Some (Pbrt.Decoder.string d)
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_descriptor_proto), field(1)" pk
    | Some (2, Pbrt.Bytes) -> v.package <- Some (Pbrt.Decoder.string d)
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_descriptor_proto), field(2)" pk
    | Some (3, Pbrt.Bytes) -> v.dependency <- Pbrt.Decoder.string d :: v.dependency
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_descriptor_proto), field(3)" pk
    | Some (10, Pbrt.Varint) ->
        v.public_dependency <- Pbrt.Decoder.int_as_varint d :: v.public_dependency
    | Some (10, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_descriptor_proto), field(10)" pk
    | Some (11, Pbrt.Varint) ->
        v.weak_dependency <- Pbrt.Decoder.int_as_varint d :: v.weak_dependency
    | Some (11, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_descriptor_proto), field(11)" pk
    | Some (4, Pbrt.Bytes) ->
        v.message_type <-
          decode_descriptor_proto (Pbrt.Decoder.nested d) :: v.message_type
    | Some (4, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_descriptor_proto), field(4)" pk
    | Some (5, Pbrt.Bytes) ->
        v.enum_type <-
          decode_enum_descriptor_proto (Pbrt.Decoder.nested d) :: v.enum_type
    | Some (5, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_descriptor_proto), field(5)" pk
    | Some (6, Pbrt.Bytes) ->
        v.service <- decode_service_descriptor_proto (Pbrt.Decoder.nested d) :: v.service
    | Some (6, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_descriptor_proto), field(6)" pk
    | Some (7, Pbrt.Bytes) ->
        v.extension <-
          decode_field_descriptor_proto (Pbrt.Decoder.nested d) :: v.extension
    | Some (7, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_descriptor_proto), field(7)" pk
    | Some (8, Pbrt.Bytes) ->
        v.options <- Some (decode_file_options (Pbrt.Decoder.nested d))
    | Some (8, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_descriptor_proto), field(8)" pk
    | Some (9, Pbrt.Bytes) ->
        v.source_code_info <- Some (decode_source_code_info (Pbrt.Decoder.nested d))
    | Some (9, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_descriptor_proto), field(9)" pk
    | Some (12, Pbrt.Bytes) -> v.syntax <- Some (Pbrt.Decoder.string d)
    | Some (12, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_descriptor_proto), field(12)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Descriptor_types.name = v.name;
     Descriptor_types.package = v.package;
     Descriptor_types.dependency = v.dependency;
     Descriptor_types.public_dependency = v.public_dependency;
     Descriptor_types.weak_dependency = v.weak_dependency;
     Descriptor_types.message_type = v.message_type;
     Descriptor_types.enum_type = v.enum_type;
     Descriptor_types.service = v.service;
     Descriptor_types.extension = v.extension;
     Descriptor_types.options = v.options;
     Descriptor_types.source_code_info = v.source_code_info;
     Descriptor_types.syntax = v.syntax;
   }
    : Descriptor_types.file_descriptor_proto)

let rec decode_file_descriptor_set d =
  let v = default_file_descriptor_set_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.file <- List.rev v.file;
        continue__ := false
    | Some (1, Pbrt.Bytes) ->
        v.file <- decode_file_descriptor_proto (Pbrt.Decoder.nested d) :: v.file
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(file_descriptor_set), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({Descriptor_types.file = v.file} : Descriptor_types.file_descriptor_set)

let rec decode_generated_code_info_annotation d =
  let v = default_generated_code_info_annotation_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.path <- List.rev v.path;
        continue__ := false
    | Some (1, Pbrt.Bytes) ->
        v.path <-
          Pbrt.Decoder.packed_fold (fun l d -> Pbrt.Decoder.int_as_varint d :: l) [] d
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload
          "Message(generated_code_info_annotation), field(1)"
          pk
    | Some (2, Pbrt.Bytes) -> v.source_file <- Some (Pbrt.Decoder.string d)
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload
          "Message(generated_code_info_annotation), field(2)"
          pk
    | Some (3, Pbrt.Varint) -> v.begin_ <- Some (Pbrt.Decoder.int_as_varint d)
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload
          "Message(generated_code_info_annotation), field(3)"
          pk
    | Some (4, Pbrt.Varint) -> v.end_ <- Some (Pbrt.Decoder.int_as_varint d)
    | Some (4, pk) ->
        Pbrt.Decoder.unexpected_payload
          "Message(generated_code_info_annotation), field(4)"
          pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Descriptor_types.path = v.path;
     Descriptor_types.source_file = v.source_file;
     Descriptor_types.begin_ = v.begin_;
     Descriptor_types.end_ = v.end_;
   }
    : Descriptor_types.generated_code_info_annotation)

let rec decode_generated_code_info d =
  let v = default_generated_code_info_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.annotation <- List.rev v.annotation;
        continue__ := false
    | Some (1, Pbrt.Bytes) ->
        v.annotation <-
          decode_generated_code_info_annotation (Pbrt.Decoder.nested d) :: v.annotation
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(generated_code_info), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({Descriptor_types.annotation = v.annotation} : Descriptor_types.generated_code_info)

let rec encode_field_descriptor_proto_label
    (v : Descriptor_types.field_descriptor_proto_label) encoder
  =
  match v with
  | Descriptor_types.Label_optional -> Pbrt.Encoder.int_as_varint 1 encoder
  | Descriptor_types.Label_required -> Pbrt.Encoder.int_as_varint 2 encoder
  | Descriptor_types.Label_repeated -> Pbrt.Encoder.int_as_varint 3 encoder

let rec encode_field_descriptor_proto_type
    (v : Descriptor_types.field_descriptor_proto_type) encoder
  =
  match v with
  | Descriptor_types.Type_double -> Pbrt.Encoder.int_as_varint 1 encoder
  | Descriptor_types.Type_float -> Pbrt.Encoder.int_as_varint 2 encoder
  | Descriptor_types.Type_int64 -> Pbrt.Encoder.int_as_varint 3 encoder
  | Descriptor_types.Type_uint64 -> Pbrt.Encoder.int_as_varint 4 encoder
  | Descriptor_types.Type_int32 -> Pbrt.Encoder.int_as_varint 5 encoder
  | Descriptor_types.Type_fixed64 -> Pbrt.Encoder.int_as_varint 6 encoder
  | Descriptor_types.Type_fixed32 -> Pbrt.Encoder.int_as_varint 7 encoder
  | Descriptor_types.Type_bool -> Pbrt.Encoder.int_as_varint 8 encoder
  | Descriptor_types.Type_string -> Pbrt.Encoder.int_as_varint 9 encoder
  | Descriptor_types.Type_group -> Pbrt.Encoder.int_as_varint 10 encoder
  | Descriptor_types.Type_message -> Pbrt.Encoder.int_as_varint 11 encoder
  | Descriptor_types.Type_bytes -> Pbrt.Encoder.int_as_varint 12 encoder
  | Descriptor_types.Type_uint32 -> Pbrt.Encoder.int_as_varint 13 encoder
  | Descriptor_types.Type_enum -> Pbrt.Encoder.int_as_varint 14 encoder
  | Descriptor_types.Type_sfixed32 -> Pbrt.Encoder.int_as_varint 15 encoder
  | Descriptor_types.Type_sfixed64 -> Pbrt.Encoder.int_as_varint 16 encoder
  | Descriptor_types.Type_sint32 -> Pbrt.Encoder.int_as_varint 17 encoder
  | Descriptor_types.Type_sint64 -> Pbrt.Encoder.int_as_varint 18 encoder

let rec encode_field_options_ctype (v : Descriptor_types.field_options_ctype) encoder =
  match v with
  | Descriptor_types.String -> Pbrt.Encoder.int_as_varint 0 encoder
  | Descriptor_types.Cord -> Pbrt.Encoder.int_as_varint 1 encoder
  | Descriptor_types.String_piece -> Pbrt.Encoder.int_as_varint 2 encoder

let rec encode_field_options_jstype (v : Descriptor_types.field_options_jstype) encoder =
  match v with
  | Descriptor_types.Js_normal -> Pbrt.Encoder.int_as_varint 0 encoder
  | Descriptor_types.Js_string -> Pbrt.Encoder.int_as_varint 1 encoder
  | Descriptor_types.Js_number -> Pbrt.Encoder.int_as_varint 2 encoder

let rec encode_uninterpreted_option_name_part
    (v : Descriptor_types.uninterpreted_option_name_part) encoder
  =
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
  Pbrt.Encoder.string v.Descriptor_types.name_part encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder;
  Pbrt.Encoder.bool v.Descriptor_types.is_extension encoder;
  ()

let rec encode_uninterpreted_option (v : Descriptor_types.uninterpreted_option) encoder =
  List.iter
    (fun x ->
      Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_uninterpreted_option_name_part x) encoder)
    v.Descriptor_types.name;
  (match v.Descriptor_types.identifier_value with
  | Some x ->
      Pbrt.Encoder.key (3, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  (match v.Descriptor_types.positive_int_value with
  | Some x ->
      Pbrt.Encoder.key (4, Pbrt.Varint) encoder;
      Pbrt.Encoder.int_as_varint x encoder
  | None -> ());
  (match v.Descriptor_types.negative_int_value with
  | Some x ->
      Pbrt.Encoder.key (5, Pbrt.Varint) encoder;
      Pbrt.Encoder.int_as_varint x encoder
  | None -> ());
  (match v.Descriptor_types.double_value with
  | Some x ->
      Pbrt.Encoder.key (6, Pbrt.Bits64) encoder;
      Pbrt.Encoder.float_as_bits64 x encoder
  | None -> ());
  (match v.Descriptor_types.string_value with
  | Some x ->
      Pbrt.Encoder.key (7, Pbrt.Bytes) encoder;
      Pbrt.Encoder.bytes x encoder
  | None -> ());
  (match v.Descriptor_types.aggregate_value with
  | Some x ->
      Pbrt.Encoder.key (8, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  ()

let rec encode_field_options (v : Descriptor_types.field_options) encoder =
  (match v.Descriptor_types.ctype with
  | Some x ->
      Pbrt.Encoder.key (1, Pbrt.Varint) encoder;
      encode_field_options_ctype x encoder
  | None -> ());
  (match v.Descriptor_types.packed with
  | Some x ->
      Pbrt.Encoder.key (2, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  (match v.Descriptor_types.jstype with
  | Some x ->
      Pbrt.Encoder.key (6, Pbrt.Varint) encoder;
      encode_field_options_jstype x encoder
  | None -> ());
  (match v.Descriptor_types.lazy_ with
  | Some x ->
      Pbrt.Encoder.key (5, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  (match v.Descriptor_types.deprecated with
  | Some x ->
      Pbrt.Encoder.key (3, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  (match v.Descriptor_types.weak with
  | Some x ->
      Pbrt.Encoder.key (10, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  List.iter
    (fun x ->
      Pbrt.Encoder.key (999, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_uninterpreted_option x) encoder)
    v.Descriptor_types.uninterpreted_option;
  ()

let rec encode_field_descriptor_proto (v : Descriptor_types.field_descriptor_proto)
    encoder
  =
  (match v.Descriptor_types.name with
  | Some x ->
      Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  (match v.Descriptor_types.number with
  | Some x ->
      Pbrt.Encoder.key (3, Pbrt.Varint) encoder;
      Pbrt.Encoder.int_as_varint x encoder
  | None -> ());
  (match v.Descriptor_types.label with
  | Some x ->
      Pbrt.Encoder.key (4, Pbrt.Varint) encoder;
      encode_field_descriptor_proto_label x encoder
  | None -> ());
  (match v.Descriptor_types.type_ with
  | Some x ->
      Pbrt.Encoder.key (5, Pbrt.Varint) encoder;
      encode_field_descriptor_proto_type x encoder
  | None -> ());
  (match v.Descriptor_types.type_name with
  | Some x ->
      Pbrt.Encoder.key (6, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  (match v.Descriptor_types.extendee with
  | Some x ->
      Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  (match v.Descriptor_types.default_value with
  | Some x ->
      Pbrt.Encoder.key (7, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  (match v.Descriptor_types.oneof_index with
  | Some x ->
      Pbrt.Encoder.key (9, Pbrt.Varint) encoder;
      Pbrt.Encoder.int_as_varint x encoder
  | None -> ());
  (match v.Descriptor_types.json_name with
  | Some x ->
      Pbrt.Encoder.key (10, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  (match v.Descriptor_types.options with
  | Some x ->
      Pbrt.Encoder.key (8, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_field_options x) encoder
  | None -> ());
  ()

let rec encode_enum_value_options (v : Descriptor_types.enum_value_options) encoder =
  (match v.Descriptor_types.deprecated with
  | Some x ->
      Pbrt.Encoder.key (1, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  List.iter
    (fun x ->
      Pbrt.Encoder.key (999, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_uninterpreted_option x) encoder)
    v.Descriptor_types.uninterpreted_option;
  ()

let rec encode_enum_value_descriptor_proto
    (v : Descriptor_types.enum_value_descriptor_proto) encoder
  =
  (match v.Descriptor_types.name with
  | Some x ->
      Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  (match v.Descriptor_types.number with
  | Some x ->
      Pbrt.Encoder.key (2, Pbrt.Varint) encoder;
      Pbrt.Encoder.int_as_varint x encoder
  | None -> ());
  (match v.Descriptor_types.options with
  | Some x ->
      Pbrt.Encoder.key (3, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_enum_value_options x) encoder
  | None -> ());
  ()

let rec encode_enum_options (v : Descriptor_types.enum_options) encoder =
  (match v.Descriptor_types.allow_alias with
  | Some x ->
      Pbrt.Encoder.key (2, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  (match v.Descriptor_types.deprecated with
  | Some x ->
      Pbrt.Encoder.key (3, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  List.iter
    (fun x ->
      Pbrt.Encoder.key (999, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_uninterpreted_option x) encoder)
    v.Descriptor_types.uninterpreted_option;
  ()

let rec encode_enum_descriptor_proto_enum_reserved_range
    (v : Descriptor_types.enum_descriptor_proto_enum_reserved_range) encoder
  =
  (match v.Descriptor_types.start with
  | Some x ->
      Pbrt.Encoder.key (1, Pbrt.Varint) encoder;
      Pbrt.Encoder.int_as_varint x encoder
  | None -> ());
  (match v.Descriptor_types.end_ with
  | Some x ->
      Pbrt.Encoder.key (2, Pbrt.Varint) encoder;
      Pbrt.Encoder.int_as_varint x encoder
  | None -> ());
  ()

let rec encode_enum_descriptor_proto (v : Descriptor_types.enum_descriptor_proto) encoder
  =
  (match v.Descriptor_types.name with
  | Some x ->
      Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  List.iter
    (fun x ->
      Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_enum_value_descriptor_proto x) encoder)
    v.Descriptor_types.value;
  (match v.Descriptor_types.options with
  | Some x ->
      Pbrt.Encoder.key (3, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_enum_options x) encoder
  | None -> ());
  List.iter
    (fun x ->
      Pbrt.Encoder.key (4, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_enum_descriptor_proto_enum_reserved_range x) encoder)
    v.Descriptor_types.reserved_range;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (5, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder)
    v.Descriptor_types.reserved_name;
  ()

let rec encode_extension_range_options (v : Descriptor_types.extension_range_options)
    encoder
  =
  List.iter
    (fun x ->
      Pbrt.Encoder.key (999, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_uninterpreted_option x) encoder)
    v.Descriptor_types.uninterpreted_option;
  ()

let rec encode_descriptor_proto_extension_range
    (v : Descriptor_types.descriptor_proto_extension_range) encoder
  =
  (match v.Descriptor_types.start with
  | Some x ->
      Pbrt.Encoder.key (1, Pbrt.Varint) encoder;
      Pbrt.Encoder.int_as_varint x encoder
  | None -> ());
  (match v.Descriptor_types.end_ with
  | Some x ->
      Pbrt.Encoder.key (2, Pbrt.Varint) encoder;
      Pbrt.Encoder.int_as_varint x encoder
  | None -> ());
  (match v.Descriptor_types.options with
  | Some x ->
      Pbrt.Encoder.key (3, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_extension_range_options x) encoder
  | None -> ());
  ()

let rec encode_oneof_options (v : Descriptor_types.oneof_options) encoder =
  List.iter
    (fun x ->
      Pbrt.Encoder.key (999, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_uninterpreted_option x) encoder)
    v.Descriptor_types.uninterpreted_option;
  ()

let rec encode_oneof_descriptor_proto (v : Descriptor_types.oneof_descriptor_proto)
    encoder
  =
  (match v.Descriptor_types.name with
  | Some x ->
      Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  (match v.Descriptor_types.options with
  | Some x ->
      Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_oneof_options x) encoder
  | None -> ());
  ()

let rec encode_message_options (v : Descriptor_types.message_options) encoder =
  (match v.Descriptor_types.message_set_wire_format with
  | Some x ->
      Pbrt.Encoder.key (1, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  (match v.Descriptor_types.no_standard_descriptor_accessor with
  | Some x ->
      Pbrt.Encoder.key (2, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  (match v.Descriptor_types.deprecated with
  | Some x ->
      Pbrt.Encoder.key (3, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  (match v.Descriptor_types.map_entry with
  | Some x ->
      Pbrt.Encoder.key (7, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  List.iter
    (fun x ->
      Pbrt.Encoder.key (999, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_uninterpreted_option x) encoder)
    v.Descriptor_types.uninterpreted_option;
  ()

let rec encode_descriptor_proto_reserved_range
    (v : Descriptor_types.descriptor_proto_reserved_range) encoder
  =
  (match v.Descriptor_types.start with
  | Some x ->
      Pbrt.Encoder.key (1, Pbrt.Varint) encoder;
      Pbrt.Encoder.int_as_varint x encoder
  | None -> ());
  (match v.Descriptor_types.end_ with
  | Some x ->
      Pbrt.Encoder.key (2, Pbrt.Varint) encoder;
      Pbrt.Encoder.int_as_varint x encoder
  | None -> ());
  ()

let rec encode_descriptor_proto (v : Descriptor_types.descriptor_proto) encoder =
  (match v.Descriptor_types.name with
  | Some x ->
      Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  List.iter
    (fun x ->
      Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_field_descriptor_proto x) encoder)
    v.Descriptor_types.field;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (6, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_field_descriptor_proto x) encoder)
    v.Descriptor_types.extension;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (3, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_descriptor_proto x) encoder)
    v.Descriptor_types.nested_type;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (4, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_enum_descriptor_proto x) encoder)
    v.Descriptor_types.enum_type;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (5, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_descriptor_proto_extension_range x) encoder)
    v.Descriptor_types.extension_range;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (8, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_oneof_descriptor_proto x) encoder)
    v.Descriptor_types.oneof_decl;
  (match v.Descriptor_types.options with
  | Some x ->
      Pbrt.Encoder.key (7, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_message_options x) encoder
  | None -> ());
  List.iter
    (fun x ->
      Pbrt.Encoder.key (9, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_descriptor_proto_reserved_range x) encoder)
    v.Descriptor_types.reserved_range;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (10, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder)
    v.Descriptor_types.reserved_name;
  ()

let rec encode_method_options_idempotency_level
    (v : Descriptor_types.method_options_idempotency_level) encoder
  =
  match v with
  | Descriptor_types.Idempotency_unknown -> Pbrt.Encoder.int_as_varint 0 encoder
  | Descriptor_types.No_side_effects -> Pbrt.Encoder.int_as_varint 1 encoder
  | Descriptor_types.Idempotent -> Pbrt.Encoder.int_as_varint 2 encoder

let rec encode_method_options (v : Descriptor_types.method_options) encoder =
  (match v.Descriptor_types.deprecated with
  | Some x ->
      Pbrt.Encoder.key (33, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  (match v.Descriptor_types.idempotency_level with
  | Some x ->
      Pbrt.Encoder.key (34, Pbrt.Varint) encoder;
      encode_method_options_idempotency_level x encoder
  | None -> ());
  List.iter
    (fun x ->
      Pbrt.Encoder.key (999, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_uninterpreted_option x) encoder)
    v.Descriptor_types.uninterpreted_option;
  ()

let rec encode_method_descriptor_proto (v : Descriptor_types.method_descriptor_proto)
    encoder
  =
  (match v.Descriptor_types.name with
  | Some x ->
      Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  (match v.Descriptor_types.input_type with
  | Some x ->
      Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  (match v.Descriptor_types.output_type with
  | Some x ->
      Pbrt.Encoder.key (3, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  (match v.Descriptor_types.options with
  | Some x ->
      Pbrt.Encoder.key (4, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_method_options x) encoder
  | None -> ());
  (match v.Descriptor_types.client_streaming with
  | Some x ->
      Pbrt.Encoder.key (5, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  (match v.Descriptor_types.server_streaming with
  | Some x ->
      Pbrt.Encoder.key (6, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  ()

let rec encode_service_options (v : Descriptor_types.service_options) encoder =
  (match v.Descriptor_types.deprecated with
  | Some x ->
      Pbrt.Encoder.key (33, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  List.iter
    (fun x ->
      Pbrt.Encoder.key (999, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_uninterpreted_option x) encoder)
    v.Descriptor_types.uninterpreted_option;
  ()

let rec encode_service_descriptor_proto (v : Descriptor_types.service_descriptor_proto)
    encoder
  =
  (match v.Descriptor_types.name with
  | Some x ->
      Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  List.iter
    (fun x ->
      Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_method_descriptor_proto x) encoder)
    v.Descriptor_types.method_;
  (match v.Descriptor_types.options with
  | Some x ->
      Pbrt.Encoder.key (3, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_service_options x) encoder
  | None -> ());
  ()

let rec encode_file_options_optimize_mode
    (v : Descriptor_types.file_options_optimize_mode) encoder
  =
  match v with
  | Descriptor_types.Speed -> Pbrt.Encoder.int_as_varint 1 encoder
  | Descriptor_types.Code_size -> Pbrt.Encoder.int_as_varint 2 encoder
  | Descriptor_types.Lite_runtime -> Pbrt.Encoder.int_as_varint 3 encoder

let rec encode_file_options (v : Descriptor_types.file_options) encoder =
  (match v.Descriptor_types.java_package with
  | Some x ->
      Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  (match v.Descriptor_types.java_outer_classname with
  | Some x ->
      Pbrt.Encoder.key (8, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  (match v.Descriptor_types.java_multiple_files with
  | Some x ->
      Pbrt.Encoder.key (10, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  (match v.Descriptor_types.java_generate_equals_and_hash with
  | Some x ->
      Pbrt.Encoder.key (20, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  (match v.Descriptor_types.java_string_check_utf8 with
  | Some x ->
      Pbrt.Encoder.key (27, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  (match v.Descriptor_types.optimize_for with
  | Some x ->
      Pbrt.Encoder.key (9, Pbrt.Varint) encoder;
      encode_file_options_optimize_mode x encoder
  | None -> ());
  (match v.Descriptor_types.go_package with
  | Some x ->
      Pbrt.Encoder.key (11, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  (match v.Descriptor_types.cc_generic_services with
  | Some x ->
      Pbrt.Encoder.key (16, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  (match v.Descriptor_types.java_generic_services with
  | Some x ->
      Pbrt.Encoder.key (17, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  (match v.Descriptor_types.py_generic_services with
  | Some x ->
      Pbrt.Encoder.key (18, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  (match v.Descriptor_types.php_generic_services with
  | Some x ->
      Pbrt.Encoder.key (42, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  (match v.Descriptor_types.deprecated with
  | Some x ->
      Pbrt.Encoder.key (23, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  (match v.Descriptor_types.cc_enable_arenas with
  | Some x ->
      Pbrt.Encoder.key (31, Pbrt.Varint) encoder;
      Pbrt.Encoder.bool x encoder
  | None -> ());
  (match v.Descriptor_types.objc_class_prefix with
  | Some x ->
      Pbrt.Encoder.key (36, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  (match v.Descriptor_types.csharp_namespace with
  | Some x ->
      Pbrt.Encoder.key (37, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  (match v.Descriptor_types.swift_prefix with
  | Some x ->
      Pbrt.Encoder.key (39, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  (match v.Descriptor_types.php_class_prefix with
  | Some x ->
      Pbrt.Encoder.key (40, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  (match v.Descriptor_types.php_namespace with
  | Some x ->
      Pbrt.Encoder.key (41, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  (match v.Descriptor_types.php_metadata_namespace with
  | Some x ->
      Pbrt.Encoder.key (44, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  (match v.Descriptor_types.ruby_package with
  | Some x ->
      Pbrt.Encoder.key (45, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  List.iter
    (fun x ->
      Pbrt.Encoder.key (999, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_uninterpreted_option x) encoder)
    v.Descriptor_types.uninterpreted_option;
  ()

let rec encode_source_code_info_location (v : Descriptor_types.source_code_info_location)
    encoder
  =
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
  Pbrt.Encoder.nested
    (fun encoder ->
      List.iter (fun x -> Pbrt.Encoder.int_as_varint x encoder) v.Descriptor_types.path)
    encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
  Pbrt.Encoder.nested
    (fun encoder ->
      List.iter (fun x -> Pbrt.Encoder.int_as_varint x encoder) v.Descriptor_types.span)
    encoder;
  (match v.Descriptor_types.leading_comments with
  | Some x ->
      Pbrt.Encoder.key (3, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  (match v.Descriptor_types.trailing_comments with
  | Some x ->
      Pbrt.Encoder.key (4, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  List.iter
    (fun x ->
      Pbrt.Encoder.key (6, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder)
    v.Descriptor_types.leading_detached_comments;
  ()

let rec encode_source_code_info (v : Descriptor_types.source_code_info) encoder =
  List.iter
    (fun x ->
      Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_source_code_info_location x) encoder)
    v.Descriptor_types.location;
  ()

let rec encode_file_descriptor_proto (v : Descriptor_types.file_descriptor_proto) encoder
  =
  (match v.Descriptor_types.name with
  | Some x ->
      Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  (match v.Descriptor_types.package with
  | Some x ->
      Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  List.iter
    (fun x ->
      Pbrt.Encoder.key (3, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder)
    v.Descriptor_types.dependency;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (10, Pbrt.Varint) encoder;
      Pbrt.Encoder.int_as_varint x encoder)
    v.Descriptor_types.public_dependency;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (11, Pbrt.Varint) encoder;
      Pbrt.Encoder.int_as_varint x encoder)
    v.Descriptor_types.weak_dependency;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (4, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_descriptor_proto x) encoder)
    v.Descriptor_types.message_type;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (5, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_enum_descriptor_proto x) encoder)
    v.Descriptor_types.enum_type;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (6, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_service_descriptor_proto x) encoder)
    v.Descriptor_types.service;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (7, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_field_descriptor_proto x) encoder)
    v.Descriptor_types.extension;
  (match v.Descriptor_types.options with
  | Some x ->
      Pbrt.Encoder.key (8, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_file_options x) encoder
  | None -> ());
  (match v.Descriptor_types.source_code_info with
  | Some x ->
      Pbrt.Encoder.key (9, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_source_code_info x) encoder
  | None -> ());
  (match v.Descriptor_types.syntax with
  | Some x ->
      Pbrt.Encoder.key (12, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  ()

let rec encode_file_descriptor_set (v : Descriptor_types.file_descriptor_set) encoder =
  List.iter
    (fun x ->
      Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_file_descriptor_proto x) encoder)
    v.Descriptor_types.file;
  ()

let rec encode_generated_code_info_annotation
    (v : Descriptor_types.generated_code_info_annotation) encoder
  =
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
  Pbrt.Encoder.nested
    (fun encoder ->
      List.iter (fun x -> Pbrt.Encoder.int_as_varint x encoder) v.Descriptor_types.path)
    encoder;
  (match v.Descriptor_types.source_file with
  | Some x ->
      Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder
  | None -> ());
  (match v.Descriptor_types.begin_ with
  | Some x ->
      Pbrt.Encoder.key (3, Pbrt.Varint) encoder;
      Pbrt.Encoder.int_as_varint x encoder
  | None -> ());
  (match v.Descriptor_types.end_ with
  | Some x ->
      Pbrt.Encoder.key (4, Pbrt.Varint) encoder;
      Pbrt.Encoder.int_as_varint x encoder
  | None -> ());
  ()

let rec encode_generated_code_info (v : Descriptor_types.generated_code_info) encoder =
  List.iter
    (fun x ->
      Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_generated_code_info_annotation x) encoder)
    v.Descriptor_types.annotation;
  ()
