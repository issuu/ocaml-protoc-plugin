(* Should we generate an ast????
   It would be simpler, and then generate the code based on that one
   The question is if this is actually harder than just parsing and emitting                                             OTOH: Its easier to create interfaces and modules once we know all the types.
   We also need to do a lexiographic ordering. No I dont!!!
*)

type t = {
  filename: string;
  package: string option;
}
