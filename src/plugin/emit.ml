open Core

let log fmt = eprintf (fmt ^^ "\n%!")

let parse request =
  log "Parse the request: %s" (Format.asprintf "%a" Spec.Plugin.Pp.pp_code_generator_request request);
  ()
