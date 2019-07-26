[@@@ocaml.warning "-27-30-39"]

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

let rec default_version ?(major : int option = None) ?(minor : int option = None)
    ?(patch : int option = None) ?(suffix : string option = None) ()
    : version
  =
  {major; minor; patch; suffix}

let rec default_code_generator_request ?(file_to_generate : string list = [])
    ?(parameter : string option = None)
    ?(proto_file : Descriptor_types.file_descriptor_proto list = [])
    ?(compiler_version : version option = None) ()
    : code_generator_request
  =
  {file_to_generate; parameter; proto_file; compiler_version}

let rec default_code_generator_response_file ?(name : string option = None)
    ?(insertion_point : string option = None) ?(content : string option = None) ()
    : code_generator_response_file
  =
  {name; insertion_point; content}

let rec default_code_generator_response ?(error : string option = None)
    ?(file : code_generator_response_file list = []) ()
    : code_generator_response
  =
  {error; file}
