(** Module to force specific deserialization strategies during tests
    though dependency injection *)

type strategy = Fast | Full | Standard [@@deriving show]
let strategy = ref Standard
let set_stragegy s = strategy := s

module Runtime' = struct
  include Ocaml_protoc_plugin.Runtime.Runtime'
  module Deserialize : module type of Deserialize = struct
    include Deserialize
    let deserialize spec constr reader =
      match !strategy with
      | Fast ->
        deserialize_fast spec constr reader
      | Full -> deserialize_full spec constr reader
      | Standard -> deserialize spec constr reader
  end
end
