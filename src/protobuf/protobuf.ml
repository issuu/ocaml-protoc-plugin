module Serialize = Serialize
module Deserialize = Deserialize
module Reader = Reader
module Writer = Writer
module Service = Service
module Result = Result

let test () =
  Writer.Test.test ();
  Serialize.Test.test ();
  LittleEndian.Test.test ();
  ()
