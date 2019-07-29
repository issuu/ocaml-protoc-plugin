(** descriptor.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_field_descriptor_proto_label : Descriptor_types.field_descriptor_proto_label -> Pbrt.Encoder.t -> unit
(** [encode_field_descriptor_proto_label v encoder] encodes [v] with the given [encoder] *)

val encode_field_descriptor_proto_type : Descriptor_types.field_descriptor_proto_type -> Pbrt.Encoder.t -> unit
(** [encode_field_descriptor_proto_type v encoder] encodes [v] with the given [encoder] *)

val encode_field_options_ctype : Descriptor_types.field_options_ctype -> Pbrt.Encoder.t -> unit
(** [encode_field_options_ctype v encoder] encodes [v] with the given [encoder] *)

val encode_field_options_jstype : Descriptor_types.field_options_jstype -> Pbrt.Encoder.t -> unit
(** [encode_field_options_jstype v encoder] encodes [v] with the given [encoder] *)

val encode_uninterpreted_option_name_part : Descriptor_types.uninterpreted_option_name_part -> Pbrt.Encoder.t -> unit
(** [encode_uninterpreted_option_name_part v encoder] encodes [v] with the given [encoder] *)

val encode_uninterpreted_option : Descriptor_types.uninterpreted_option -> Pbrt.Encoder.t -> unit
(** [encode_uninterpreted_option v encoder] encodes [v] with the given [encoder] *)

val encode_field_options : Descriptor_types.field_options -> Pbrt.Encoder.t -> unit
(** [encode_field_options v encoder] encodes [v] with the given [encoder] *)

val encode_field_descriptor_proto : Descriptor_types.field_descriptor_proto -> Pbrt.Encoder.t -> unit
(** [encode_field_descriptor_proto v encoder] encodes [v] with the given [encoder] *)

val encode_enum_value_options : Descriptor_types.enum_value_options -> Pbrt.Encoder.t -> unit
(** [encode_enum_value_options v encoder] encodes [v] with the given [encoder] *)

val encode_enum_value_descriptor_proto : Descriptor_types.enum_value_descriptor_proto -> Pbrt.Encoder.t -> unit
(** [encode_enum_value_descriptor_proto v encoder] encodes [v] with the given [encoder] *)

val encode_enum_options : Descriptor_types.enum_options -> Pbrt.Encoder.t -> unit
(** [encode_enum_options v encoder] encodes [v] with the given [encoder] *)

val encode_enum_descriptor_proto_enum_reserved_range : Descriptor_types.enum_descriptor_proto_enum_reserved_range -> Pbrt.Encoder.t -> unit
(** [encode_enum_descriptor_proto_enum_reserved_range v encoder] encodes [v] with the given [encoder] *)

val encode_enum_descriptor_proto : Descriptor_types.enum_descriptor_proto -> Pbrt.Encoder.t -> unit
(** [encode_enum_descriptor_proto v encoder] encodes [v] with the given [encoder] *)

val encode_extension_range_options : Descriptor_types.extension_range_options -> Pbrt.Encoder.t -> unit
(** [encode_extension_range_options v encoder] encodes [v] with the given [encoder] *)

val encode_descriptor_proto_extension_range : Descriptor_types.descriptor_proto_extension_range -> Pbrt.Encoder.t -> unit
(** [encode_descriptor_proto_extension_range v encoder] encodes [v] with the given [encoder] *)

val encode_oneof_options : Descriptor_types.oneof_options -> Pbrt.Encoder.t -> unit
(** [encode_oneof_options v encoder] encodes [v] with the given [encoder] *)

val encode_oneof_descriptor_proto : Descriptor_types.oneof_descriptor_proto -> Pbrt.Encoder.t -> unit
(** [encode_oneof_descriptor_proto v encoder] encodes [v] with the given [encoder] *)

val encode_message_options : Descriptor_types.message_options -> Pbrt.Encoder.t -> unit
(** [encode_message_options v encoder] encodes [v] with the given [encoder] *)

val encode_descriptor_proto_reserved_range : Descriptor_types.descriptor_proto_reserved_range -> Pbrt.Encoder.t -> unit
(** [encode_descriptor_proto_reserved_range v encoder] encodes [v] with the given [encoder] *)

val encode_descriptor_proto : Descriptor_types.descriptor_proto -> Pbrt.Encoder.t -> unit
(** [encode_descriptor_proto v encoder] encodes [v] with the given [encoder] *)

val encode_method_options_idempotency_level : Descriptor_types.method_options_idempotency_level -> Pbrt.Encoder.t -> unit
(** [encode_method_options_idempotency_level v encoder] encodes [v] with the given [encoder] *)

val encode_method_options : Descriptor_types.method_options -> Pbrt.Encoder.t -> unit
(** [encode_method_options v encoder] encodes [v] with the given [encoder] *)

val encode_method_descriptor_proto : Descriptor_types.method_descriptor_proto -> Pbrt.Encoder.t -> unit
(** [encode_method_descriptor_proto v encoder] encodes [v] with the given [encoder] *)

val encode_service_options : Descriptor_types.service_options -> Pbrt.Encoder.t -> unit
(** [encode_service_options v encoder] encodes [v] with the given [encoder] *)

val encode_service_descriptor_proto : Descriptor_types.service_descriptor_proto -> Pbrt.Encoder.t -> unit
(** [encode_service_descriptor_proto v encoder] encodes [v] with the given [encoder] *)

val encode_file_options_optimize_mode : Descriptor_types.file_options_optimize_mode -> Pbrt.Encoder.t -> unit
(** [encode_file_options_optimize_mode v encoder] encodes [v] with the given [encoder] *)

val encode_file_options : Descriptor_types.file_options -> Pbrt.Encoder.t -> unit
(** [encode_file_options v encoder] encodes [v] with the given [encoder] *)

val encode_source_code_info_location : Descriptor_types.source_code_info_location -> Pbrt.Encoder.t -> unit
(** [encode_source_code_info_location v encoder] encodes [v] with the given [encoder] *)

val encode_source_code_info : Descriptor_types.source_code_info -> Pbrt.Encoder.t -> unit
(** [encode_source_code_info v encoder] encodes [v] with the given [encoder] *)

val encode_file_descriptor_proto : Descriptor_types.file_descriptor_proto -> Pbrt.Encoder.t -> unit
(** [encode_file_descriptor_proto v encoder] encodes [v] with the given [encoder] *)

val encode_file_descriptor_set : Descriptor_types.file_descriptor_set -> Pbrt.Encoder.t -> unit
(** [encode_file_descriptor_set v encoder] encodes [v] with the given [encoder] *)

val encode_generated_code_info_annotation : Descriptor_types.generated_code_info_annotation -> Pbrt.Encoder.t -> unit
(** [encode_generated_code_info_annotation v encoder] encodes [v] with the given [encoder] *)

val encode_generated_code_info : Descriptor_types.generated_code_info -> Pbrt.Encoder.t -> unit
(** [encode_generated_code_info v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_field_descriptor_proto_label : Pbrt.Decoder.t -> Descriptor_types.field_descriptor_proto_label
(** [decode_field_descriptor_proto_label decoder] decodes a [field_descriptor_proto_label] value from [decoder] *)

val decode_field_descriptor_proto_type : Pbrt.Decoder.t -> Descriptor_types.field_descriptor_proto_type
(** [decode_field_descriptor_proto_type decoder] decodes a [field_descriptor_proto_type] value from [decoder] *)

val decode_field_options_ctype : Pbrt.Decoder.t -> Descriptor_types.field_options_ctype
(** [decode_field_options_ctype decoder] decodes a [field_options_ctype] value from [decoder] *)

val decode_field_options_jstype : Pbrt.Decoder.t -> Descriptor_types.field_options_jstype
(** [decode_field_options_jstype decoder] decodes a [field_options_jstype] value from [decoder] *)

val decode_uninterpreted_option_name_part : Pbrt.Decoder.t -> Descriptor_types.uninterpreted_option_name_part
(** [decode_uninterpreted_option_name_part decoder] decodes a [uninterpreted_option_name_part] value from [decoder] *)

val decode_uninterpreted_option : Pbrt.Decoder.t -> Descriptor_types.uninterpreted_option
(** [decode_uninterpreted_option decoder] decodes a [uninterpreted_option] value from [decoder] *)

val decode_field_options : Pbrt.Decoder.t -> Descriptor_types.field_options
(** [decode_field_options decoder] decodes a [field_options] value from [decoder] *)

val decode_field_descriptor_proto : Pbrt.Decoder.t -> Descriptor_types.field_descriptor_proto
(** [decode_field_descriptor_proto decoder] decodes a [field_descriptor_proto] value from [decoder] *)

val decode_enum_value_options : Pbrt.Decoder.t -> Descriptor_types.enum_value_options
(** [decode_enum_value_options decoder] decodes a [enum_value_options] value from [decoder] *)

val decode_enum_value_descriptor_proto : Pbrt.Decoder.t -> Descriptor_types.enum_value_descriptor_proto
(** [decode_enum_value_descriptor_proto decoder] decodes a [enum_value_descriptor_proto] value from [decoder] *)

val decode_enum_options : Pbrt.Decoder.t -> Descriptor_types.enum_options
(** [decode_enum_options decoder] decodes a [enum_options] value from [decoder] *)

val decode_enum_descriptor_proto_enum_reserved_range : Pbrt.Decoder.t -> Descriptor_types.enum_descriptor_proto_enum_reserved_range
(** [decode_enum_descriptor_proto_enum_reserved_range decoder] decodes a [enum_descriptor_proto_enum_reserved_range] value from [decoder] *)

val decode_enum_descriptor_proto : Pbrt.Decoder.t -> Descriptor_types.enum_descriptor_proto
(** [decode_enum_descriptor_proto decoder] decodes a [enum_descriptor_proto] value from [decoder] *)

val decode_extension_range_options : Pbrt.Decoder.t -> Descriptor_types.extension_range_options
(** [decode_extension_range_options decoder] decodes a [extension_range_options] value from [decoder] *)

val decode_descriptor_proto_extension_range : Pbrt.Decoder.t -> Descriptor_types.descriptor_proto_extension_range
(** [decode_descriptor_proto_extension_range decoder] decodes a [descriptor_proto_extension_range] value from [decoder] *)

val decode_oneof_options : Pbrt.Decoder.t -> Descriptor_types.oneof_options
(** [decode_oneof_options decoder] decodes a [oneof_options] value from [decoder] *)

val decode_oneof_descriptor_proto : Pbrt.Decoder.t -> Descriptor_types.oneof_descriptor_proto
(** [decode_oneof_descriptor_proto decoder] decodes a [oneof_descriptor_proto] value from [decoder] *)

val decode_message_options : Pbrt.Decoder.t -> Descriptor_types.message_options
(** [decode_message_options decoder] decodes a [message_options] value from [decoder] *)

val decode_descriptor_proto_reserved_range : Pbrt.Decoder.t -> Descriptor_types.descriptor_proto_reserved_range
(** [decode_descriptor_proto_reserved_range decoder] decodes a [descriptor_proto_reserved_range] value from [decoder] *)

val decode_descriptor_proto : Pbrt.Decoder.t -> Descriptor_types.descriptor_proto
(** [decode_descriptor_proto decoder] decodes a [descriptor_proto] value from [decoder] *)

val decode_method_options_idempotency_level : Pbrt.Decoder.t -> Descriptor_types.method_options_idempotency_level
(** [decode_method_options_idempotency_level decoder] decodes a [method_options_idempotency_level] value from [decoder] *)

val decode_method_options : Pbrt.Decoder.t -> Descriptor_types.method_options
(** [decode_method_options decoder] decodes a [method_options] value from [decoder] *)

val decode_method_descriptor_proto : Pbrt.Decoder.t -> Descriptor_types.method_descriptor_proto
(** [decode_method_descriptor_proto decoder] decodes a [method_descriptor_proto] value from [decoder] *)

val decode_service_options : Pbrt.Decoder.t -> Descriptor_types.service_options
(** [decode_service_options decoder] decodes a [service_options] value from [decoder] *)

val decode_service_descriptor_proto : Pbrt.Decoder.t -> Descriptor_types.service_descriptor_proto
(** [decode_service_descriptor_proto decoder] decodes a [service_descriptor_proto] value from [decoder] *)

val decode_file_options_optimize_mode : Pbrt.Decoder.t -> Descriptor_types.file_options_optimize_mode
(** [decode_file_options_optimize_mode decoder] decodes a [file_options_optimize_mode] value from [decoder] *)

val decode_file_options : Pbrt.Decoder.t -> Descriptor_types.file_options
(** [decode_file_options decoder] decodes a [file_options] value from [decoder] *)

val decode_source_code_info_location : Pbrt.Decoder.t -> Descriptor_types.source_code_info_location
(** [decode_source_code_info_location decoder] decodes a [source_code_info_location] value from [decoder] *)

val decode_source_code_info : Pbrt.Decoder.t -> Descriptor_types.source_code_info
(** [decode_source_code_info decoder] decodes a [source_code_info] value from [decoder] *)

val decode_file_descriptor_proto : Pbrt.Decoder.t -> Descriptor_types.file_descriptor_proto
(** [decode_file_descriptor_proto decoder] decodes a [file_descriptor_proto] value from [decoder] *)

val decode_file_descriptor_set : Pbrt.Decoder.t -> Descriptor_types.file_descriptor_set
(** [decode_file_descriptor_set decoder] decodes a [file_descriptor_set] value from [decoder] *)

val decode_generated_code_info_annotation : Pbrt.Decoder.t -> Descriptor_types.generated_code_info_annotation
(** [decode_generated_code_info_annotation decoder] decodes a [generated_code_info_annotation] value from [decoder] *)

val decode_generated_code_info : Pbrt.Decoder.t -> Descriptor_types.generated_code_info
(** [decode_generated_code_info decoder] decodes a [generated_code_info] value from [decoder] *)
