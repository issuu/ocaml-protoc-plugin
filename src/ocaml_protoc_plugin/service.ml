module type Message = sig
  type t
  val from_proto: Reader.t -> t Result.t
  val to_proto: t -> Writer.t
end

module type Rpc = sig
  module Request : Message
  module Response : Message
  val name : string
end

let make_client_functions (type req) (type rep)
    ((module Request : Message with type t = req),
     (module Response : Message with type t = rep)) =
  Request.to_proto, Response.from_proto

let make_client_functions' (type req) (type rep)
    (module R : Rpc with type Request.t = req and type Response.t = rep) =
  make_client_functions ((module R.Request), (module R.Response))

let make_service_functions (type req) (type rep)
    ((module Request : Message with type t = req),
    (module Response : Message with type t = rep)) =
  Request.from_proto, Response.to_proto

let make_service_functions' (type req) (type rep)
    (module R : Rpc with type Request.t = req and type Response.t = rep) =
  make_service_functions ((module R.Request),
    (module R.Response))

