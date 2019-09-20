module type Message = sig
  type t
  val from_proto: Reader.t -> t Deserialize.result
  val to_proto: t -> Writer.t
end

let make_client_functions (type req) (type rep)
    ((module Request : Message with type t = req),
     (module Reply : Message with type t = rep)) =
  Request.to_proto, Reply.from_proto

let make_service_functions (type req) (type rep)
    ((module Request : Message with type t = req),
    (module Reply : Message with type t = rep)) =
  Request.from_proto, Reply.to_proto
