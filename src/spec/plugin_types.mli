(** plugin.proto Types *)

(** {2 Types} *)

type version = {
  major : int option;
  minor : int option;
  patch : int option;
  suffix : string option;
}

type code_generator_request = {
  file_to_generate : string list;
  parameter : string option;
  proto_file : Descriptor_types.file_descriptor_proto list;
  compiler_version : version option;
}

type code_generator_response_file = {
  name : string option;
  insertion_point : string option;
  content : string option;
}

type code_generator_response = {
  error : string option;
  file : code_generator_response_file list;
}

(** {2 Default values} *)

(** [default_version ()] is the default value for type [version] *)
val default_version
  :  ?major:int option ->
  ?minor:int option ->
  ?patch:int option ->
  ?suffix:string option ->
  unit ->
  version

(** [default_code_generator_request ()] is the default value for type [code_generator_request] *)
val default_code_generator_request
  :  ?file_to_generate:string list ->
  ?parameter:string option ->
  ?proto_file:Descriptor_types.file_descriptor_proto list ->
  ?compiler_version:version option ->
  unit ->
  code_generator_request

(** [default_code_generator_response_file ()] is the default value for type [code_generator_response_file] *)
val default_code_generator_response_file
  :  ?name:string option ->
  ?insertion_point:string option ->
  ?content:string option ->
  unit ->
  code_generator_response_file

(** [default_code_generator_response ()] is the default value for type [code_generator_response] *)
val default_code_generator_response
  :  ?error:string option ->
  ?file:code_generator_response_file list ->
  unit ->
  code_generator_response
