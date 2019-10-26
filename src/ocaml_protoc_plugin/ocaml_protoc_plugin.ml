(**/**)
module Serialize = Serialize
module Deserialize = Deserialize
module Spec = Spec
module Runtime = Runtime
(**/**)

module Reader = Reader
module Writer = Writer
module Service = Service
module Result = Result
module Extensions = Extensions

(**/**)

let test () =
  Writer.Test.test ();
  Serialize.Test.test ();
  LittleEndian.Test.test ();
  ()
(**/**)
