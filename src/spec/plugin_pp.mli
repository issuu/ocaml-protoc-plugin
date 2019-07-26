(** plugin.proto Pretty Printing *)

(** {2 Formatters} *)

(** [pp_version v] formats v *)
val pp_version : Format.formatter -> Plugin_types.version -> unit

(** [pp_code_generator_request v] formats v *)
val pp_code_generator_request
  :  Format.formatter ->
  Plugin_types.code_generator_request ->
  unit

(** [pp_code_generator_response_file v] formats v *)
val pp_code_generator_response_file
  :  Format.formatter ->
  Plugin_types.code_generator_response_file ->
  unit

(** [pp_code_generator_response v] formats v *)
val pp_code_generator_response
  :  Format.formatter ->
  Plugin_types.code_generator_response ->
  unit
