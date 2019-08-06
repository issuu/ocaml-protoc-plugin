(** plugin.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_version : Plugin_types.version -> Pbrt.Encoder.t -> unit
(** [encode_version v encoder] encodes [v] with the given [encoder] *)

val encode_code_generator_request : Plugin_types.code_generator_request -> Pbrt.Encoder.t -> unit
(** [encode_code_generator_request v encoder] encodes [v] with the given [encoder] *)

val encode_code_generator_response_file : Plugin_types.code_generator_response_file -> Pbrt.Encoder.t -> unit
(** [encode_code_generator_response_file v encoder] encodes [v] with the given [encoder] *)

val encode_code_generator_response : Plugin_types.code_generator_response -> Pbrt.Encoder.t -> unit
(** [encode_code_generator_response v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_version : Pbrt.Decoder.t -> Plugin_types.version
(** [decode_version decoder] decodes a [version] value from [decoder] *)

val decode_code_generator_request : Pbrt.Decoder.t -> Plugin_types.code_generator_request
(** [decode_code_generator_request decoder] decodes a [code_generator_request] value from [decoder] *)

val decode_code_generator_response_file : Pbrt.Decoder.t -> Plugin_types.code_generator_response_file
(** [decode_code_generator_response_file decoder] decodes a [code_generator_response_file] value from [decoder] *)

val decode_code_generator_response : Pbrt.Decoder.t -> Plugin_types.code_generator_response
(** [decode_code_generator_response decoder] decodes a [code_generator_response] value from [decoder] *)
